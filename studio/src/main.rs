#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod app;
mod audio;
mod bridge;
mod theme;
mod widgets;

use app::SonoraStudioApp;
use eframe::egui;

fn main() -> eframe::Result<()> {
    let native_options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_title("Sonora Studio — Functional Audio Synthesizer & Composition Suite")
            .with_inner_size([1220.0, 780.0])
            .with_min_inner_size([980.0, 640.0])
            .with_active(true),
        ..Default::default()
    };

    eframe::run_native(
        "Sonora Studio",
        native_options,
        Box::new(|cc| Ok(Box::new(SonoraStudioApp::new(cc)))),
    )
}
