use egui::{Color32, CornerRadius, Margin, RichText, Stroke};
use std::env;
use std::path::PathBuf;

use crate::audio::AudioPlayer;
use crate::bridge::{InspectResponse, SonoraBridge};
use crate::theme::StudioTheme;
use crate::widgets::{
    AdsrCanvas, AdsrParams, FilterCurve, FilterMode, ParseErrorMarker, RotaryKnob,
    ScoreEditor, VirtualKeyboard, WaveformScope,
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StudioTab {
    ScoreStudio,
    PatchDesigner,
    AlgorithmicComposer,
    Mixer,
}

#[allow(dead_code)]
pub struct SonoraStudioApp {
    current_tab: StudioTab,
    bridge: SonoraBridge,
    audio: AudioPlayer,

    // Score Editor State
    score_code: String,
    inspect_result: Option<InspectResponse>,
    parse_error: Option<ParseErrorMarker>,
    current_song_path: Option<PathBuf>,

    // Transport & Playback
    master_volume: f32,
    bpm: f32,
    playhead_sec: f32,
    status_message: String,
    is_busy: bool,

    // Patch Designer State
    current_preset: String,
    osc1_wave: usize,
    osc1_detune: f32,
    osc1_mix: f32,
    osc1_pw: f32,

    osc2_wave: usize,
    osc2_detune: f32,
    osc2_mix: f32,

    sub_noise_mix: f32,

    filter_mode: FilterMode,
    filter_cutoff: f32,
    filter_resonance: f32,

    amp_adsr: AdsrParams,
    filter_adsr: AdsrParams,

    // Effects
    reverb_on: bool,
    reverb_size: f32,
    reverb_damp: f32,
    reverb_wet: f32,

    delay_on: bool,
    delay_time: f32,
    delay_feedback: f32,
    delay_wet: f32,

    chorus_on: bool,
    chorus_rate: f32,
    chorus_depth: f32,

    drive_on: bool,
    drive_gain: f32,

    master_pan: f32,

    // Virtual Keyboard
    audition_note: Option<String>,
    last_auditioned_note: Option<String>,

    // Procedural Generator State
    seed: u64,
    scale_root: String,
    scale_mode: String,
    bars: usize,

    // Mixer Track Levels
    track_volumes: Vec<f32>,
    track_pans: Vec<f32>,
    track_mutes: Vec<bool>,
    track_solos: Vec<bool>,
}

impl SonoraStudioApp {
    pub fn new(_cc: &eframe::CreationContext) -> Self {
        let default_score = include_str!("../../examples/synthwave.sonora").to_string();

        let bridge = SonoraBridge::new();
        let mut app = Self {
            current_tab: StudioTab::ScoreStudio,
            bridge,
            audio: AudioPlayer::new(),

            score_code: default_score,
            inspect_result: None,
            parse_error: None,
            current_song_path: None,

            master_volume: 1.0,
            bpm: 124.0,
            playhead_sec: 0.0,
            status_message: "Sonora Engine ready.".to_string(),
            is_busy: false,

            current_preset: "lead".to_string(),
            osc1_wave: 1, // Saw
            osc1_detune: -7.0,
            osc1_mix: 0.70,
            osc1_pw: 0.50,

            osc2_wave: 2, // Square
            osc2_detune: 7.0,
            osc2_mix: 0.55,

            sub_noise_mix: 0.15,

            filter_mode: FilterMode::LowPass,
            filter_cutoff: 2400.0,
            filter_resonance: 1.8,

            amp_adsr: AdsrParams { attack: 0.015, decay: 0.18, sustain: 0.65, release: 0.28 },
            filter_adsr: AdsrParams { attack: 0.02, decay: 0.25, sustain: 0.35, release: 0.20 },

            reverb_on: true,
            reverb_size: 0.65,
            reverb_damp: 0.35,
            reverb_wet: 0.25,

            delay_on: true,
            delay_time: 320.0,
            delay_feedback: 0.40,
            delay_wet: 0.30,

            chorus_on: true,
            chorus_rate: 1.5,
            chorus_depth: 0.30,

            drive_on: false,
            drive_gain: 1.2,

            master_pan: 0.0,

            audition_note: None,
            last_auditioned_note: None,

            seed: 12345,
            scale_root: "D".to_string(),
            scale_mode: "Dorian".to_string(),
            bars: 8,

            track_volumes: vec![1.0, 0.9, 0.95],
            track_pans: vec![0.0, -0.3, 0.3],
            track_mutes: vec![false, false, false],
            track_solos: vec![false, false, false],
        };

        app.run_inspect();
        app
    }

    fn run_inspect(&mut self) {
        match self.bridge.inspect_score(&self.score_code) {
            Ok(resp) => {
                if resp.status == "ok" {
                    if let Some(b) = resp.bpm { self.bpm = b; }
                    self.parse_error = None;
                    self.status_message = format!("✓ Score parsed successfully. (Tracks: {})", resp.track_count.unwrap_or(0));
                    self.inspect_result = Some(resp);
                } else {
                    self.parse_error = Some(ParseErrorMarker {
                        line: resp.line.unwrap_or(1),
                        column: resp.column.unwrap_or(1),
                        message: resp.message.unwrap_or_else(|| "Syntax error".to_string()),
                    });
                    self.status_message = "⚠ Syntax error in score".to_string();
                }
            }
            Err(e) => {
                self.status_message = format!("Parse check: {}", e);
            }
        }
    }

    fn render_and_play(&mut self) {
        self.status_message = "Synthesizing audio via Haskell engine...".to_string();
        self.is_busy = true;

        let temp_wav = env::temp_dir().join("sonora_studio_active.wav");
        match self.bridge.render_score(&self.score_code, &temp_wav, 44100, "16") {
            Ok(()) => {
                if let Err(e) = self.audio.load_wav(&temp_wav) {
                    self.status_message = format!("Audio load error: {}", e);
                } else {
                    let _ = self.audio.play(0.0);
                    self.status_message = format!("▶ Playing: {:.1}s rendered buffer", self.audio.duration_sec);
                }
            }
            Err(e) => {
                self.status_message = format!("Render failed: {}", e);
            }
        }
        self.is_busy = false;
    }

    fn trigger_note_audition(&mut self, pitch: String) {
        let temp_wav = env::temp_dir().join("sonora_note_preview.wav");
        let cutoff = Some(self.filter_cutoff);
        let res = Some(self.filter_resonance);
        let adsr = Some(&self.amp_adsr);

        if let Ok(()) = self.bridge.synth_note_preview(&self.current_preset, &pitch, 0.8, cutoff, res, adsr, &temp_wav) {
            let _ = self.audio.load_wav(&temp_wav);
            let _ = self.audio.play(0.0);
            self.status_message = format!("Auditioning {} ({})", pitch, self.current_preset);
        }
    }

    fn export_wav(&mut self) {
        if let Some(path) = rfd::FileDialog::new()
            .add_filter("WAV Audio", &["wav"])
            .set_file_name("sonora_export.wav")
            .save_file()
        {
            self.status_message = "Exporting production WAV...".to_string();
            match self.bridge.render_score(&self.score_code, &path, 48000, "24") {
                Ok(()) => {
                    self.status_message = format!("✓ Successfully exported 24-bit 48kHz WAV to: {}", path.display());
                }
                Err(e) => {
                    self.status_message = format!("Export failed: {}", e);
                }
            }
        }
    }
}

impl eframe::App for SonoraStudioApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        StudioTheme::apply(ctx);

        if self.audio.is_playing() {
            self.playhead_sec = self.audio.playhead();
            ctx.request_repaint();
        }

        let note_to_audition = self.audition_note.clone();
        if let Some(ref note) = note_to_audition {
            if self.last_auditioned_note.as_ref() != Some(note) {
                let p = note.clone();
                self.trigger_note_audition(p);
                self.last_auditioned_note = Some(note.clone());
            }
        }

        // TOP TRANSPORT & NAVIGATION BAR
        egui::TopBottomPanel::top("top_transport")
            .frame(egui::Frame::NONE.fill(StudioTheme::BG_PANEL).inner_margin(Margin::symmetric(16, 10)))
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.label(RichText::new("SONORA").strong().size(18.0).color(StudioTheme::ACCENT_CYAN));
                    ui.label(RichText::new("STUDIO").size(14.0).color(StudioTheme::TEXT_MUTED));
                    ui.add_space(16.0);

                    let tabs = [
                        (StudioTab::ScoreStudio, "📝 Score Studio"),
                        (StudioTab::PatchDesigner, "🎛 Patch Designer"),
                        (StudioTab::AlgorithmicComposer, "🎲 Algorithmic"),
                        (StudioTab::Mixer, "🎚 Multi-Track Mixer"),
                    ];

                    for (tab, label) in tabs {
                        let is_active = self.current_tab == tab;
                        let text = RichText::new(label)
                            .color(if is_active { StudioTheme::ACCENT_CYAN } else { StudioTheme::TEXT_SECONDARY })
                            .strong();
                        if ui.selectable_label(is_active, text).clicked() {
                            self.current_tab = tab;
                        }
                    }

                    ui.add_space(24.0);

                    let is_playing = self.audio.is_playing();
                    let play_btn_text = if is_playing { "⏸ Pause" } else { "▶ Play" };
                    if ui.button(RichText::new(play_btn_text).strong().color(Color32::WHITE)).clicked() {
                        if is_playing {
                            self.audio.pause();
                        } else {
                            if self.audio.duration_sec > 0.0 {
                                let _ = self.audio.play(self.playhead_sec);
                            } else {
                                self.render_and_play();
                            }
                        }
                    }

                    if ui.button(RichText::new("⏹ Stop").color(StudioTheme::TEXT_PRIMARY)).clicked() {
                        self.audio.stop();
                        self.playhead_sec = 0.0;
                    }

                    let is_loop = self.audio.is_looping();
                    if ui.selectable_label(is_loop, "🔁 Loop").clicked() {
                        self.audio.set_looping(!is_loop);
                    }

                    ui.add_space(16.0);

                    if ui.button(RichText::new("⚡ Render & Play").strong().color(StudioTheme::ACCENT_CYAN)).clicked() {
                        self.render_and_play();
                    }

                    if ui.button(RichText::new("💾 Export WAV").color(StudioTheme::TEXT_PRIMARY)).clicked() {
                        self.export_wav();
                    }

                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                        let mut vol_display = self.master_volume * 100.0;
                        if ui.add(RotaryKnob::new(&mut vol_display, 0.0, 150.0, 100.0, "MASTER").unit("%").size(34.0)).changed() {
                            self.master_volume = vol_display / 100.0;
                            self.audio.set_volume(self.master_volume);
                        }

                        ui.add_space(12.0);
                        let mut bpm_val = self.bpm;
                        if ui.add(RotaryKnob::new(&mut bpm_val, 40.0, 240.0, 120.0, "TEMPO").unit("BPM").size(34.0)).changed() {
                            self.bpm = bpm_val;
                        }
                    });
                });
            });

        // BOTTOM STATUS & LOG BAR
        egui::TopBottomPanel::bottom("bottom_status")
            .frame(egui::Frame::NONE.fill(StudioTheme::BG_OBSIDIAN).inner_margin(Margin::symmetric(14, 6)))
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    let status_color = if self.status_message.starts_with('⚠') {
                        StudioTheme::ACCENT_RED
                    } else if self.status_message.starts_with('✓') || self.status_message.starts_with('▶') {
                        StudioTheme::ACCENT_CYAN
                    } else {
                        StudioTheme::TEXT_MUTED
                    };

                    ui.label(RichText::new(&self.status_message).color(status_color).small());

                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                        let engine_info = "Sonora Pure Haskell Audio Engine (ZDF SVF, SplitMix64, 44.1/48kHz Stereo)";
                        ui.label(RichText::new(engine_info).color(StudioTheme::TEXT_MUTED).small());
                    });
                });
            });

        // MAIN WORKSPACE PANEL
        egui::CentralPanel::default()
            .frame(egui::Frame::NONE.fill(StudioTheme::BG_OBSIDIAN).inner_margin(Margin::same(14)))
            .show(ctx, |ui| {
                match self.current_tab {
                    StudioTab::ScoreStudio => self.ui_score_studio(ui),
                    StudioTab::PatchDesigner => self.ui_patch_designer(ui),
                    StudioTab::AlgorithmicComposer => self.ui_algorithmic_composer(ui),
                    StudioTab::Mixer => self.ui_mixer(ui),
                }
            });
    }
}

impl SonoraStudioApp {
    fn ui_score_studio(&mut self, ui: &mut egui::Ui) {
        ui.columns(2, |cols| {
            cols[0].vertical(|ui| {
                ui.horizontal(|ui| {
                    ui.label(RichText::new("TEMPLATES:").color(StudioTheme::TEXT_MUTED).small());
                    if ui.button("Synthwave").clicked() {
                        self.score_code = include_str!("../../examples/synthwave.sonora").to_string();
                        self.run_inspect();
                    }
                    if ui.button("Bach Prelude").clicked() {
                        self.score_code = include_str!("../../examples/bach_prelude.sonora").to_string();
                        self.run_inspect();
                    }
                    if ui.button("Ambient Drone").clicked() {
                        self.score_code = include_str!("../../examples/ambient.sonora").to_string();
                        self.run_inspect();
                    }
                });

                ui.add_space(6.0);

                let editor_res = ui.add(ScoreEditor::new(&mut self.score_code, self.parse_error.as_ref()));
                if editor_res.changed() {
                    self.run_inspect();
                }
            });

            cols[1].vertical(|ui| {
                ui.label(RichText::new("SCORE STRUCTURE & TRACKS").strong().color(StudioTheme::TEXT_PRIMARY));
                ui.add_space(4.0);

                egui::Frame::NONE
                    .fill(StudioTheme::BG_CARD)
                    .corner_radius(CornerRadius::same(8))
                    .stroke(Stroke::new(1.0, StudioTheme::BORDER_DARK))
                    .inner_margin(Margin::same(12))
                    .show(ui, |ui| {
                        if let Some(ref insp) = self.inspect_result {
                            ui.horizontal(|ui| {
                                ui.label(RichText::new(format!("Title: {}", insp.title.as_deref().unwrap_or("Untitled"))).strong());
                                ui.label(format!("| Tempo: {:.0} BPM", insp.bpm.unwrap_or(120.0)));
                                ui.label(format!("| Time: {}", insp.time_signature.as_deref().unwrap_or("4/4")));
                            });

                            ui.separator();

                            if let Some(ref tracks) = insp.tracks {
                                for t in tracks {
                                    ui.horizontal(|ui| {
                                        ui.label(RichText::new(&t.name).color(StudioTheme::ACCENT_CYAN).strong());
                                        ui.label(format!("(preset: {})", t.instrument));
                                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                                            ui.label(format!("{:.1}s | {} notes", t.duration_seconds, t.notes_count));
                                        });
                                    });
                                }
                            }
                        } else {
                            ui.label(RichText::new("Enter valid .sonora score to view track breakdown.").color(StudioTheme::TEXT_MUTED));
                        }
                    });

                ui.add_space(10.0);
                ui.label(RichText::new("AUDIO WAVEFORM & PLAYHEAD").strong().color(StudioTheme::TEXT_PRIMARY));
                ui.add_space(4.0);

                let scope_res = ui.add(
                    WaveformScope::new(&self.audio.peaks, self.audio.duration_sec, &mut self.playhead_sec)
                        .height(130.0)
                        .accent(StudioTheme::ACCENT_CYAN),
                );

                if scope_res.changed() {
                    self.audio.seek(self.playhead_sec);
                }
            });
        });
    }

    fn ui_patch_designer(&mut self, ui: &mut egui::Ui) {
        ui.horizontal(|ui| {
            ui.label(RichText::new("PRESET:").strong().color(StudioTheme::TEXT_PRIMARY));
            let presets = ["lead", "bass", "pad", "pluck", "chime", "piano"];
            for &p in &presets {
                if ui.selectable_label(self.current_preset == p, p).clicked() {
                    self.current_preset = p.to_string();
                    match p {
                        "lead" => {
                            self.filter_cutoff = 3000.0;
                            self.filter_resonance = 1.5;
                            self.amp_adsr = AdsrParams { attack: 0.01, decay: 0.10, sustain: 0.70, release: 0.20 };
                        }
                        "bass" => {
                            self.filter_cutoff = 800.0;
                            self.filter_resonance = 2.2;
                            self.amp_adsr = AdsrParams { attack: 0.005, decay: 0.12, sustain: 0.80, release: 0.10 };
                        }
                        "pad" => {
                            self.filter_cutoff = 1800.0;
                            self.filter_resonance = 1.0;
                            self.amp_adsr = AdsrParams { attack: 0.35, decay: 0.50, sustain: 0.80, release: 0.70 };
                        }
                        "pluck" => {
                            self.filter_cutoff = 4000.0;
                            self.filter_resonance = 2.0;
                            self.amp_adsr = AdsrParams { attack: 0.002, decay: 0.22, sustain: 0.10, release: 0.15 };
                        }
                        _ => {}
                    }
                }
            }
        });

        ui.add_space(8.0);

        ui.horizontal_top(|ui| {
            // MODULE 1: MULTI-OSCILLATOR MIXER
            egui::Frame::NONE
                .fill(StudioTheme::BG_CARD)
                .corner_radius(CornerRadius::same(8))
                .stroke(Stroke::new(1.0, StudioTheme::BORDER_DARK))
                .inner_margin(Margin::same(12))
                .show(ui, |ui| {
                    ui.set_width(220.0);
                    ui.label(RichText::new("OSCILLATOR MIXER").strong().color(StudioTheme::ACCENT_CYAN));
                    ui.separator();

                    ui.label(RichText::new("Osc 1 (Primary)").small().color(StudioTheme::TEXT_MUTED));
                    ui.horizontal(|ui| {
                        ui.add(RotaryKnob::new(&mut self.osc1_mix, 0.0, 1.0, 0.7, "MIX").unit("").size(34.0));
                        ui.add(RotaryKnob::new(&mut self.osc1_detune, -24.0, 24.0, 0.0, "TUNE").unit("st").size(34.0));
                        ui.add(RotaryKnob::new(&mut self.osc1_pw, 0.1, 0.9, 0.5, "PW").unit("").size(34.0));
                    });

                    ui.add_space(6.0);
                    ui.label(RichText::new("Osc 2 (Detuned)").small().color(StudioTheme::TEXT_MUTED));
                    ui.horizontal(|ui| {
                        ui.add(RotaryKnob::new(&mut self.osc2_mix, 0.0, 1.0, 0.5, "MIX").unit("").size(34.0));
                        ui.add(RotaryKnob::new(&mut self.osc2_detune, -24.0, 24.0, 7.0, "TUNE").unit("st").size(34.0));
                        ui.add(RotaryKnob::new(&mut self.sub_noise_mix, 0.0, 1.0, 0.15, "SUB").unit("").size(34.0));
                    });
                });

            // MODULE 2: CYTOMIC SVF FILTER
            egui::Frame::NONE
                .fill(StudioTheme::BG_CARD)
                .corner_radius(CornerRadius::same(8))
                .stroke(Stroke::new(1.0, StudioTheme::BORDER_DARK))
                .inner_margin(Margin::same(12))
                .show(ui, |ui| {
                    ui.set_width(280.0);
                    ui.horizontal(|ui| {
                        ui.label(RichText::new("CYTOMIC SVF FILTER").strong().color(StudioTheme::ACCENT_AMBER));
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            let modes = [FilterMode::LowPass, FilterMode::HighPass, FilterMode::BandPass, FilterMode::Notch];
                            for m in modes {
                                let label = match m {
                                    FilterMode::LowPass => "LP",
                                    FilterMode::HighPass => "HP",
                                    FilterMode::BandPass => "BP",
                                    FilterMode::Notch => "BR",
                                };
                                if ui.selectable_label(self.filter_mode == m, label).clicked() {
                                    self.filter_mode = m;
                                }
                            }
                        });
                    });

                    ui.separator();
                    ui.add(FilterCurve::new(&mut self.filter_cutoff, &mut self.filter_resonance, self.filter_mode).height(110.0));

                    ui.horizontal(|ui| {
                        ui.add(RotaryKnob::new(&mut self.filter_cutoff, 20.0, 20000.0, 2500.0, "CUTOFF").unit("Hz").log(true).accent(StudioTheme::ACCENT_AMBER).size(34.0));
                        ui.add(RotaryKnob::new(&mut self.filter_resonance, 0.2, 10.0, 1.5, "RES (Q)").unit("").accent(StudioTheme::ACCENT_AMBER).size(34.0));
                    });
                });

            // MODULE 3: DUAL ADSR ENVELOPES
            egui::Frame::NONE
                .fill(StudioTheme::BG_CARD)
                .corner_radius(CornerRadius::same(8))
                .stroke(Stroke::new(1.0, StudioTheme::BORDER_DARK))
                .inner_margin(Margin::same(12))
                .show(ui, |ui| {
                    ui.set_width(260.0);
                    ui.label(RichText::new("AMPLITUDE ADSR ENVELOPE").strong().color(StudioTheme::ACCENT_CYAN));
                    ui.separator();
                    ui.add(AdsrCanvas::new(&mut self.amp_adsr).height(100.0).accent(StudioTheme::ACCENT_CYAN));

                    ui.add_space(4.0);
                    ui.label(RichText::new("FILTER ADSR ENVELOPE").strong().color(StudioTheme::ACCENT_AMBER));
                    ui.add(AdsrCanvas::new(&mut self.filter_adsr).height(75.0).accent(StudioTheme::ACCENT_AMBER));
                });

            // MODULE 4: STUDIO EFFECTS RACK
            egui::Frame::NONE
                .fill(StudioTheme::BG_CARD)
                .corner_radius(CornerRadius::same(8))
                .stroke(Stroke::new(1.0, StudioTheme::BORDER_DARK))
                .inner_margin(Margin::same(12))
                .show(ui, |ui| {
                    ui.set_width(240.0);
                    ui.label(RichText::new("STUDIO EFFECTS RACK").strong().color(StudioTheme::ACCENT_VIOLET));
                    ui.separator();

                    ui.horizontal(|ui| {
                        ui.checkbox(&mut self.reverb_on, "Reverb");
                        ui.add(RotaryKnob::new(&mut self.reverb_size, 0.0, 1.0, 0.6, "SIZE").unit("").accent(StudioTheme::ACCENT_VIOLET).size(30.0));
                        ui.add(RotaryKnob::new(&mut self.reverb_wet, 0.0, 1.0, 0.25, "WET").unit("").accent(StudioTheme::ACCENT_VIOLET).size(30.0));
                    });

                    ui.horizontal(|ui| {
                        ui.checkbox(&mut self.delay_on, "Delay");
                        ui.add(RotaryKnob::new(&mut self.delay_time, 20.0, 800.0, 300.0, "TIME").unit("ms").accent(StudioTheme::ACCENT_VIOLET).size(30.0));
                        ui.add(RotaryKnob::new(&mut self.delay_wet, 0.0, 1.0, 0.30, "WET").unit("").accent(StudioTheme::ACCENT_VIOLET).size(30.0));
                    });

                    ui.horizontal(|ui| {
                        ui.checkbox(&mut self.drive_on, "Drive");
                        ui.add(RotaryKnob::new(&mut self.drive_gain, 1.0, 5.0, 1.2, "CLIP").unit("").accent(StudioTheme::ACCENT_VIOLET).size(30.0));
                        ui.add(RotaryKnob::new(&mut self.master_pan, -1.0, 1.0, 0.0, "PAN").unit("").accent(StudioTheme::ACCENT_VIOLET).size(30.0));
                    });
                });
        });

        ui.add_space(12.0);

        ui.label(RichText::new("VIRTUAL PIANO KEYBOARD (Click or hold keys to audition patch)").strong().color(StudioTheme::TEXT_PRIMARY));
        ui.add_space(4.0);
        ui.add(VirtualKeyboard::new(&mut self.audition_note).height(85.0).start_octave(3));
    }

    fn ui_algorithmic_composer(&mut self, ui: &mut egui::Ui) {
        ui.columns(2, |cols| {
            cols[0].vertical(|ui| {
                ui.label(RichText::new("ALGORITHMIC GENERATOR PARAMETERS").strong().color(StudioTheme::ACCENT_GREEN));
                ui.add_space(8.0);

                egui::Frame::NONE
                    .fill(StudioTheme::BG_CARD)
                    .corner_radius(CornerRadius::same(8))
                    .stroke(Stroke::new(1.0, StudioTheme::BORDER_DARK))
                    .inner_margin(Margin::same(16))
                    .show(ui, |ui| {
                        ui.horizontal(|ui| {
                            ui.label("RNG Seed:");
                            let mut seed_str = self.seed.to_string();
                            if ui.text_edit_singleline(&mut seed_str).changed() {
                                if let Ok(s) = seed_str.parse::<u64>() {
                                    self.seed = s;
                                }
                            }
                            if ui.button("🎲 Roll Dice").clicked() {
                                use std::time::SystemTime;
                                self.seed = SystemTime::now()
                                    .duration_since(SystemTime::UNIX_EPOCH)
                                    .map(|d| d.as_nanos() as u64)
                                    .unwrap_or(42);
                            }
                        });

                        ui.add_space(8.0);

                        ui.horizontal(|ui| {
                            ui.label("Root Note:");
                            let roots = ["C", "C#", "D", "Eb", "E", "F", "F#", "G", "Ab", "A", "Bb", "B"];
                            for &r in &roots[0..6] {
                                if ui.selectable_label(self.scale_root == r, r).clicked() {
                                    self.scale_root = r.to_string();
                                }
                            }
                        });
                        ui.horizontal(|ui| {
                            ui.add_space(68.0);
                            let roots = ["C", "C#", "D", "Eb", "E", "F", "F#", "G", "Ab", "A", "Bb", "B"];
                            for &r in &roots[6..12] {
                                if ui.selectable_label(self.scale_root == r, r).clicked() {
                                    self.scale_root = r.to_string();
                                }
                            }
                        });

                        ui.add_space(8.0);

                        ui.horizontal(|ui| {
                            ui.label("Scale Mode:");
                            let modes = ["Major", "NaturalMinor", "HarmonicMinor", "Dorian", "Phrygian", "Lydian", "Mixolydian", "Pentatonic", "Blues"];
                            egui::ComboBox::from_id_salt("scale_mode_combo")
                                .selected_text(&self.scale_mode)
                                .show_ui(ui, |ui| {
                                    for &m in &modes {
                                        ui.selectable_value(&mut self.scale_mode, m.to_string(), m);
                                    }
                                });
                        });

                        ui.add_space(8.0);

                        ui.horizontal(|ui| {
                            ui.label("Length (Bars):");
                            ui.add(egui::Slider::new(&mut self.bars, 2..=32));
                        });

                        ui.add_space(14.0);

                        if ui.button(RichText::new("✨ GENERATE COMPOSITION").strong().size(14.0).color(Color32::WHITE)).clicked() {
                            let scale_str = format!("{}-{}", self.scale_root, self.scale_mode);
                            self.status_message = format!("Generating composition (Seed {})...", self.seed);

                            match self.bridge.generate_procedural(self.seed, &scale_str, self.bars, self.bpm as f64) {
                                Ok(resp) => {
                                    self.score_code = resp.dsl;
                                    self.run_inspect();
                                    self.render_and_play();
                                    self.status_message = format!("✓ Generated: {} (Seed {})", resp.title, self.seed);
                                }
                                Err(e) => {
                                    self.status_message = format!("Generation error: {}", e);
                                }
                            }
                        }
                    });
            });

            cols[1].vertical(|ui| {
                ui.label(RichText::new("GENERATED AUDIO WAVEFORM").strong().color(StudioTheme::TEXT_PRIMARY));
                ui.add_space(4.0);

                let scope_res = ui.add(
                    WaveformScope::new(&self.audio.peaks, self.audio.duration_sec, &mut self.playhead_sec)
                        .height(160.0)
                        .accent(StudioTheme::ACCENT_GREEN),
                );

                if scope_res.changed() {
                    self.audio.seek(self.playhead_sec);
                }

                ui.add_space(10.0);
                ui.label(RichText::new("SplitMix64 Markov melody engine with diatonic voice leading.").color(StudioTheme::TEXT_MUTED).small());
            });
        });
    }
}

#[inline]
fn format_db(level: f32) -> String {
    if level > 0.0 {
        format!("{:.1} dB", 20.0 * level.log10())
    } else {
        String::from("-inf")
    }
}

impl SonoraStudioApp {
    fn ui_mixer(&mut self, ui: &mut egui::Ui) {
        ui.label(RichText::new("MULTI-TRACK CHANNEL STRIPS & MASTER BUS").strong().color(StudioTheme::TEXT_PRIMARY));
        ui.add_space(8.0);

        let track_count = self.inspect_result.as_ref().and_then(|i| i.tracks.as_ref()).map(|t| t.len()).unwrap_or(0);

        while self.track_volumes.len() < track_count { self.track_volumes.push(1.0); }
        while self.track_pans.len() < track_count { self.track_pans.push(0.0); }
        while self.track_mutes.len() < track_count { self.track_mutes.push(false); }
        while self.track_solos.len() < track_count { self.track_solos.push(false); }

        ui.horizontal_top(|ui| {
            if let Some(ref insp) = self.inspect_result {
                if let Some(ref tracks) = insp.tracks {
                    for (i, t) in tracks.iter().enumerate() {
                        egui::Frame::NONE
                            .fill(StudioTheme::BG_CARD)
                            .corner_radius(CornerRadius::same(8))
                            .stroke(Stroke::new(1.0, StudioTheme::BORDER_DARK))
                            .inner_margin(Margin::same(12))
                            .show(ui, |ui| {
                                ui.set_width(120.0);
                                ui.vertical_centered(|ui| {
                                    ui.label(RichText::new(&t.name).strong().color(StudioTheme::ACCENT_CYAN));
                                    ui.label(RichText::new(&t.instrument).small().color(StudioTheme::TEXT_MUTED));
                                    ui.separator();

                                    // Pan dial
                                    ui.add(RotaryKnob::new(&mut self.track_pans[i], -1.0, 1.0, 0.0, "PAN").unit("").size(28.0));
                                    ui.add_space(4.0);

                                    // Volume Fader
                                    let mut vol = self.track_volumes[i];
                                    ui.add(egui::Slider::new(&mut vol, 0.0..=1.5).vertical());
                                    self.track_volumes[i] = vol;

                                    let db_str = format_db(vol);
                                    ui.label(RichText::new(db_str).monospace().small());

                                    ui.separator();

                                    // Mute & Solo
                                    ui.horizontal(|ui| {
                                        let mute = &mut self.track_mutes[i];
                                        if ui.selectable_label(*mute, RichText::new("M").color(if *mute { StudioTheme::ACCENT_RED } else { StudioTheme::TEXT_PRIMARY })).clicked() {
                                            *mute = !*mute;
                                        }

                                        let solo = &mut self.track_solos[i];
                                        if ui.selectable_label(*solo, RichText::new("S").color(if *solo { StudioTheme::ACCENT_AMBER } else { StudioTheme::TEXT_PRIMARY })).clicked() {
                                            *solo = !*solo;
                                        }
                                    });
                                });
                            });
                    }
                }
            } else {
                ui.label(RichText::new("No score loaded to mix.").color(StudioTheme::TEXT_MUTED));
            }

            // Master Channel Strip
            egui::Frame::NONE
                .fill(StudioTheme::BG_CARD_HOVER)
                .corner_radius(CornerRadius::same(8))
                .stroke(Stroke::new(1.5, StudioTheme::ACCENT_CYAN))
                .inner_margin(Margin::same(12))
                .show(ui, |ui| {
                    ui.set_width(130.0);
                    ui.vertical_centered(|ui| {
                        ui.label(RichText::new("MASTER BUS").strong().color(Color32::WHITE));
                        ui.label(RichText::new("Limiter ON").small().color(StudioTheme::ACCENT_CYAN));
                        ui.separator();

                        ui.add(RotaryKnob::new(&mut self.master_pan, -1.0, 1.0, 0.0, "BAL").unit("").size(28.0));
                        ui.add_space(4.0);

                        ui.add(egui::Slider::new(&mut self.master_volume, 0.0..=1.5).vertical());
                        let db_str = format_db(self.master_volume);
                        ui.label(RichText::new(db_str).monospace().small());

                        ui.separator();
                        ui.label(RichText::new("0.95 CEIL").color(StudioTheme::TEXT_MUTED).small());
                    });
                });
        });
    }
}
