use egui::{Color32, Response, Ui, Widget};

#[derive(Clone, Debug, Default)]
pub struct ParseErrorMarker {
    pub line: usize,
    pub column: usize,
    pub message: String,
}

pub struct ScoreEditor<'a> {
    text: &'a mut String,
    error: Option<&'a ParseErrorMarker>,
}

impl<'a> ScoreEditor<'a> {
    pub fn new(text: &'a mut String, error: Option<&'a ParseErrorMarker>) -> Self {
        Self { text, error }
    }
}

impl<'a> Widget for ScoreEditor<'a> {
    fn ui(self, ui: &mut Ui) -> Response {
        ui.vertical(|ui| {
            // Header bar with status
            ui.horizontal(|ui| {
                ui.label(egui::RichText::new("SCORE CODE (.sonora)").strong().color(Color32::from_rgb(160, 170, 185)));

                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    if let Some(err) = self.error {
                        let err_text = format!("⚠ Line {}:{} — {}", err.line, err.column, err.message);
                        ui.label(egui::RichText::new(err_text).color(Color32::from_rgb(230, 57, 70)).small());
                    } else {
                        ui.label(egui::RichText::new("✓ Valid Syntax").color(Color32::from_rgb(46, 196, 182)).small());
                    }
                });
            });

            ui.add_space(4.0);

            // Multiline Editor with monospace code font
            egui::ScrollArea::vertical()
                .max_height(340.0)
                .show(ui, |ui| {
                    ui.add(
                        egui::TextEdit::multiline(self.text)
                            .font(egui::TextStyle::Monospace)
                            .desired_rows(16)
                            .desired_width(f32::INFINITY)
                            .code_editor()
                            .lock_focus(true),
                    )
                }).inner
        }).inner
    }
}
