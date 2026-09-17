use rodio::{Decoder, OutputStream, OutputStreamHandle, Sink};
use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

pub struct AudioPlayer {
    _stream: Option<OutputStream>,
    stream_handle: Option<OutputStreamHandle>,
    sink: Option<Sink>,
    current_wav: Option<std::path::PathBuf>,
    pub peaks: Vec<f32>,
    pub duration_sec: f32,
    play_start_time: Option<Instant>,
    play_start_offset: f32,
    is_looping: Arc<AtomicBool>,
}

#[allow(dead_code)]
impl AudioPlayer {
    pub fn new() -> Self {
        let (_stream, stream_handle) = match OutputStream::try_default() {
            Ok((s, h)) => (Some(s), Some(h)),
            Err(e) => {
                eprintln!("[AudioPlayer] Could not open audio output stream: {}", e);
                (None, None)
            }
        };

        Self {
            _stream,
            stream_handle,
            sink: None,
            current_wav: None,
            peaks: Vec::new(),
            duration_sec: 0.0,
            play_start_time: None,
            play_start_offset: 0.0,
            is_looping: Arc::new(AtomicBool::new(false)),
        }
    }

    /// Load and extract waveform peaks from a WAV file
    pub fn load_wav(&mut self, path: &Path) -> Result<(), String> {
        let mut reader = hound::WavReader::open(path)
            .map_err(|e| format!("Failed to read WAV file: {}", e))?;

        let spec = reader.spec();
        let num_channels = spec.channels as usize;
        let sample_rate = spec.sample_rate as f32;
        let total_samples = reader.len() as f32 / num_channels as f32;
        self.duration_sec = total_samples / sample_rate;

        // Downsample to 256 visual peak bars
        let target_peaks = 256;
        let samples_per_bar = ((reader.len() as usize / target_peaks).max(1)) * num_channels;

        let mut peaks = Vec::with_capacity(target_peaks);
        let mut current_max: f32 = 0.0;
        let mut count = 0;

        match spec.sample_format {
            hound::SampleFormat::Int => {
                let max_int = (1i64 << (spec.bits_per_sample - 1)) as f32;
                for sample in reader.samples::<i32>() {
                    if let Ok(s) = sample {
                        let norm = (s as f32 / max_int).abs();
                        if norm > current_max { current_max = norm; }
                        count += 1;
                        if count >= samples_per_bar {
                            peaks.push(current_max.min(1.0));
                            current_max = 0.0;
                            count = 0;
                        }
                    }
                }
            }
            hound::SampleFormat::Float => {
                for sample in reader.samples::<f32>() {
                    if let Ok(s) = sample {
                        let norm = s.abs();
                        if norm > current_max { current_max = norm; }
                        count += 1;
                        if count >= samples_per_bar {
                            peaks.push(current_max.min(1.0));
                            current_max = 0.0;
                            count = 0;
                        }
                    }
                }
            }
        }

        if count > 0 {
            peaks.push(current_max.min(1.0));
        }

        self.peaks = peaks;
        self.current_wav = Some(path.to_path_buf());
        Ok(())
    }

    /// Start or restart playback from an offset
    pub fn play(&mut self, offset_sec: f32) -> Result<(), String> {
        let path = self.current_wav.as_ref().ok_or("No audio loaded")?;
        let handle = self.stream_handle.as_ref().ok_or("No audio device available")?;

        // Stop current sink if any
        if let Some(s) = self.sink.take() {
            s.stop();
        }

        let sink = Sink::try_new(handle).map_err(|e| e.to_string())?;
        let file = File::open(path).map_err(|e| e.to_string())?;
        let source = Decoder::new(BufReader::new(file)).map_err(|e| e.to_string())?;

        sink.append(source);
        sink.play();

        self.sink = Some(sink);
        self.play_start_time = Some(Instant::now());
        self.play_start_offset = offset_sec;

        Ok(())
    }

    pub fn pause(&mut self) {
        if let Some(ref sink) = self.sink {
            sink.pause();
        }
    }

    pub fn resume(&mut self) {
        if let Some(ref sink) = self.sink {
            sink.play();
        }
    }

    pub fn stop(&mut self) {
        if let Some(ref sink) = self.sink {
            sink.stop();
        }
        self.sink = None;
        self.play_start_time = None;
        self.play_start_offset = 0.0;
    }

    pub fn is_playing(&self) -> bool {
        if let Some(ref sink) = self.sink {
            !sink.is_paused() && !sink.empty()
        } else {
            false
        }
    }

    pub fn set_volume(&mut self, volume: f32) {
        if let Some(ref sink) = self.sink {
            sink.set_volume(volume.clamp(0.0, 2.0));
        }
    }

    pub fn set_looping(&mut self, looping: bool) {
        self.is_looping.store(looping, Ordering::Relaxed);
    }

    pub fn is_looping(&self) -> bool {
        self.is_looping.load(Ordering::Relaxed)
    }

    /// Update and get current playhead position in seconds
    pub fn playhead(&mut self) -> f32 {
        if let Some(start) = self.play_start_time {
            if self.is_playing() {
                let elapsed = start.elapsed().as_secs_f32() + self.play_start_offset;
                if elapsed >= self.duration_sec && self.duration_sec > 0.0 {
                    if self.is_looping() {
                        let _ = self.play(0.0);
                        return 0.0;
                    } else {
                        self.stop();
                        return self.duration_sec;
                    }
                }
                return elapsed.min(self.duration_sec);
            }
        }
        self.play_start_offset
    }

    pub fn seek(&mut self, target_sec: f32) {
        self.play_start_offset = target_sec.clamp(0.0, self.duration_sec);
        if self.is_playing() {
            let _ = self.play(self.play_start_offset);
        }
    }
}
