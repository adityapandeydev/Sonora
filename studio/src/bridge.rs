use serde::Deserialize;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use crate::widgets::adsr::AdsrParams;

#[derive(Clone, Debug, Deserialize)]
pub struct TrackInfo {
    pub name: String,
    pub instrument: String,
    #[serde(rename = "notesCount")]
    pub notes_count: usize,
    #[serde(rename = "durationSeconds")]
    pub duration_seconds: f32,
}

#[derive(Clone, Debug, Deserialize)]
pub struct InspectResponse {
    pub status: String,
    pub title: Option<String>,
    pub bpm: Option<f32>,
    #[serde(rename = "timeSignature")]
    pub time_signature: Option<String>,
    #[serde(rename = "trackCount")]
    pub track_count: Option<usize>,
    pub tracks: Option<Vec<TrackInfo>>,
    // Error fields
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub message: Option<String>,
}

#[allow(dead_code)]
#[derive(Clone, Debug, Deserialize)]
pub struct GenerateResponse {
    pub status: String,
    pub seed: u64,
    pub scale: String,
    pub bars: usize,
    pub bpm: f64,
    pub title: String,
    pub dsl: String,
}

pub struct SonoraBridge {
    exe_path: Option<PathBuf>,
}

impl SonoraBridge {
    pub fn new() -> Self {
        let exe = Self::locate_sonora_exe();
        Self { exe_path: exe }
    }

    fn locate_sonora_exe() -> Option<PathBuf> {
        let candidates = [
            // Standard Cabal build artifact on Windows
            PathBuf::from(r"..\dist-newstyle\build\x86_64-windows\ghc-9.10.3\sonora-0.1.0.0\x\sonora\build\sonora\sonora.exe"),
            PathBuf::from(r".\dist-newstyle\build\x86_64-windows\ghc-9.10.3\sonora-0.1.0.0\x\sonora\build\sonora\sonora.exe"),
            PathBuf::from(r"E:\Aditya\Projects\Sonora\dist-newstyle\build\x86_64-windows\ghc-9.10.3\sonora-0.1.0.0\x\sonora\build\sonora\sonora.exe"),
            PathBuf::from("sonora.exe"),
        ];

        for c in &candidates {
            if c.exists() {
                return Some(c.clone());
            }
        }

        // Check if in current exe's directory
        if let Ok(curr_exe) = env::current_exe() {
            if let Some(parent) = curr_exe.parent() {
                let p = parent.join("sonora.exe");
                if p.exists() {
                    return Some(p);
                }
            }
        }

        None
    }

    fn run_sonora_command(&self, args: &[&str]) -> Result<String, String> {
        if let Some(ref exe) = self.exe_path {
            let output = Command::new(exe)
                .args(args)
                .output()
                .map_err(|e| format!("Failed to execute sonora.exe: {}", e))?;

            let stdout = String::from_utf8_lossy(&output.stdout).to_string();
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();

            if output.status.success() {
                Ok(stdout)
            } else if !stdout.trim().is_empty() && stdout.contains("{\"status\":\"error\"") {
                // Return stdout JSON even if exit code is 1 (for parse error markers)
                Ok(stdout)
            } else {
                Err(if !stderr.is_empty() { stderr } else { stdout })
            }
        } else {
            // Fallback to cabal run
            let mut cabal_args = vec!["run", "sonora", "--"];
            cabal_args.extend_from_slice(args);
            let output = Command::new("cabal")
                .args(&cabal_args)
                .output()
                .map_err(|e| format!("Neither sonora.exe nor cabal was found: {}", e))?;

            let stdout = String::from_utf8_lossy(&output.stdout).to_string();
            if output.status.success() || stdout.contains("{\"status\":\"error\"") {
                Ok(stdout)
            } else {
                Err(String::from_utf8_lossy(&output.stderr).to_string())
            }
        }
    }

    /// Inspect a score string by writing a temporary file
    pub fn inspect_score(&self, code: &str) -> Result<InspectResponse, String> {
        let temp_dir = env::temp_dir();
        let temp_file = temp_dir.join("sonora_studio_inspect.sonora");
        fs::write(&temp_file, code).map_err(|e| e.to_string())?;

        let temp_str = temp_file.to_string_lossy();
        let out = self.run_sonora_command(&["inspect", &temp_str, "--json"])?;

        // Parse JSON from output
        let json_line = out.lines()
            .find(|l| l.trim_start().starts_with('{'))
            .ok_or_else(|| format!("No JSON object in output: {}", out))?;

        serde_json::from_str::<InspectResponse>(json_line)
            .map_err(|e| format!("JSON deserialization failed: {} (Raw: {})", e, json_line))
    }

    /// Render a score string to an output WAV file
    pub fn render_score(&self, code: &str, out_path: &Path, sample_rate: usize, bit_depth: &str) -> Result<(), String> {
        let temp_dir = env::temp_dir();
        let temp_file = temp_dir.join("sonora_studio_render.sonora");
        fs::write(&temp_file, code).map_err(|e| e.to_string())?;

        let temp_str = temp_file.to_string_lossy();
        let out_str = out_path.to_string_lossy();
        let sr_str = sample_rate.to_string();

        self.run_sonora_command(&[
            "render",
            &temp_str,
            "-o",
            &out_str,
            "--sample-rate",
            &sr_str,
            "--bit-depth",
            bit_depth,
        ])?;

        Ok(())
    }

    /// Deterministically generate procedural music score and AST
    pub fn generate_procedural(&self, seed: u64, scale: &str, bars: usize, bpm: f64) -> Result<GenerateResponse, String> {
        let seed_str = seed.to_string();
        let bars_str = bars.to_string();
        let bpm_str = format!("{:.1}", bpm);

        let out = self.run_sonora_command(&[
            "generate",
            "--seed",
            &seed_str,
            "--scale",
            scale,
            "--bars",
            &bars_str,
            "--bpm",
            &bpm_str,
            "--json",
        ])?;

        let json_line = out.lines()
            .find(|l| l.trim_start().starts_with('{'))
            .ok_or_else(|| format!("No JSON object in output: {}", out))?;

        serde_json::from_str::<GenerateResponse>(json_line)
            .map_err(|e| format!("JSON deserialization failed: {}", e))
    }

    /// Synthesize a single note preview with custom synth and envelope parameters
    pub fn synth_note_preview(
        &self,
        preset: &str,
        pitch: &str,
        duration: f32,
        cutoff: Option<f32>,
        resonance: Option<f32>,
        adsr: Option<&AdsrParams>,
        out_wav: &Path,
    ) -> Result<(), String> {
        let dur_str = format!("{:.2}", duration);
        let out_str = out_wav.to_string_lossy();

        let mut args = vec![
            "synth",
            "--preset", preset,
            "--pitch", pitch,
            "--duration", &dur_str,
            "-o", &out_str,
        ];

        let cutoff_str;
        if let Some(c) = cutoff {
            cutoff_str = format!("{:.1}", c);
            args.push("--cutoff");
            args.push(&cutoff_str);
        }

        let res_str;
        if let Some(r) = resonance {
            res_str = format!("{:.2}", r);
            args.push("--resonance");
            args.push(&res_str);
        }

        let att_str;
        let dec_str;
        let sus_str;
        let rel_str;

        if let Some(env) = adsr {
            att_str = format!("{:.4}", env.attack);
            dec_str = format!("{:.4}", env.decay);
            sus_str = format!("{:.4}", env.sustain);
            rel_str = format!("{:.4}", env.release);

            args.push("--attack"); args.push(&att_str);
            args.push("--decay"); args.push(&dec_str);
            args.push("--sustain"); args.push(&sus_str);
            args.push("--release"); args.push(&rel_str);
        }

        self.run_sonora_command(&args)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bridge_inspect() {
        let bridge = SonoraBridge::new();
        let sample = "title \"Test\"\ntempo 120\ntime 4/4\n\ntrack test (piano):\n    C4 quarter\n";
        let res = bridge.inspect_score(sample);
        assert!(res.is_ok(), "Inspect failed: {:?}", res.err());
        let info = res.unwrap();
        assert_eq!(info.status, "ok");
        assert_eq!(info.bpm, Some(120.0));
        assert_eq!(info.track_count, Some(1));
    }
}

