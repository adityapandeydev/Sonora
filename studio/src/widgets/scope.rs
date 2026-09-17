use egui::{Color32, CornerRadius, Pos2, Response, Sense, Stroke, StrokeKind, Ui, Vec2, Widget};

pub struct WaveformScope<'a> {
    peaks: &'a [f32],
    duration_sec: f32,
    playhead_sec: &'a mut f32,
    height: f32,
    accent: Color32,
}

impl<'a> WaveformScope<'a> {
    pub fn new(peaks: &'a [f32], duration_sec: f32, playhead_sec: &'a mut f32) -> Self {
        Self {
            peaks,
            duration_sec,
            playhead_sec,
            height: 100.0,
            accent: Color32::from_rgb(0, 242, 254),
        }
    }

    pub fn height(mut self, h: f32) -> Self {
        self.height = h;
        self
    }

    pub fn accent(mut self, color: Color32) -> Self {
        self.accent = color;
        self
    }
}

impl<'a> Widget for WaveformScope<'a> {
    fn ui(self, ui: &mut Ui) -> Response {
        let width = ui.available_width().max(200.0);
        let size = Vec2::new(width, self.height);
        let (rect, mut response) = ui.allocate_exact_size(size, Sense::click_and_drag());

        if (response.clicked() || response.dragged()) && self.duration_sec > 0.0 {
            if let Some(pos) = response.interact_pointer_pos() {
                let norm = ((pos.x - rect.left()) / rect.width()).clamp(0.0, 1.0);
                *self.playhead_sec = norm * self.duration_sec;
                response.mark_changed();
            }
        }

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();

            painter.rect(
                rect,
                CornerRadius::same(6),
                Color32::from_rgb(16, 18, 22),
                Stroke::new(1.0, Color32::from_rgb(34, 40, 50)),
                StrokeKind::Inside,
            );

            let center_y = rect.center().y;
            let half_h = (rect.height() - 16.0) * 0.5;

            painter.line_segment(
                [Pos2::new(rect.left(), center_y), Pos2::new(rect.right(), center_y)],
                Stroke::new(1.0, Color32::from_rgb(32, 38, 48)),
            );

            if !self.peaks.is_empty() {
                let num_peaks = self.peaks.len();
                let bar_step = rect.width() / (num_peaks as f32);

                for (i, &amp) in self.peaks.iter().enumerate() {
                    let x = rect.left() + (i as f32) * bar_step;
                    let h = (amp * half_h).clamp(1.0, half_h);

                    let norm_pos = if self.duration_sec > 0.0 {
                        *self.playhead_sec / self.duration_sec
                    } else {
                        0.0
                    };
                    let bar_norm = i as f32 / num_peaks as f32;

                    let color = if bar_norm <= norm_pos {
                        self.accent
                    } else {
                        Color32::from_rgb(65, 75, 90)
                    };

                    painter.line_segment(
                        [Pos2::new(x, center_y - h), Pos2::new(x, center_y + h)],
                        Stroke::new(1.5, color),
                    );
                }
            } else {
                painter.text(
                    rect.center(),
                    egui::Align2::CENTER_CENTER,
                    "No audio rendered yet. Press [Render & Play] to synthesize.",
                    egui::FontId::proportional(11.0),
                    Color32::from_rgb(90, 100, 115),
                );
            }

            if self.duration_sec > 0.0 {
                let norm = (*self.playhead_sec / self.duration_sec).clamp(0.0, 1.0);
                let px = rect.left() + norm * rect.width();

                painter.line_segment(
                    [Pos2::new(px, rect.top() + 2.0), Pos2::new(px, rect.bottom() - 2.0)],
                    Stroke::new(2.0, Color32::WHITE),
                );

                painter.circle_filled(Pos2::new(px, rect.top() + 6.0), 3.5, self.accent);
            }

            let time_str = format!("{:.1}s / {:.1}s", *self.playhead_sec, self.duration_sec);
            painter.text(
                Pos2::new(rect.right() - 8.0, rect.bottom() - 8.0),
                egui::Align2::RIGHT_BOTTOM,
                time_str,
                egui::FontId::monospace(9.5),
                Color32::from_rgb(140, 150, 165),
            );
        }

        response
    }
}
