use egui::{Color32, CornerRadius, Pos2, Rect, Response, Sense, Stroke, StrokeKind, Ui, Vec2, Widget};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FilterMode {
    LowPass,
    HighPass,
    BandPass,
    Notch,
}

pub struct FilterCurve<'a> {
    cutoff: &'a mut f32, // Hz (20.0 .. 20000.0)
    resonance: &'a mut f32, // Q (0.2 .. 10.0)
    mode: FilterMode,
    height: f32,
    accent: Color32,
}

impl<'a> FilterCurve<'a> {
    pub fn new(cutoff: &'a mut f32, resonance: &'a mut f32, mode: FilterMode) -> Self {
        Self {
            cutoff,
            resonance,
            mode,
            height: 140.0,
            accent: Color32::from_rgb(255, 159, 28), // Electric Amber
        }
    }

    pub fn height(mut self, height: f32) -> Self {
        self.height = height;
        self
    }
}

impl<'a> Widget for FilterCurve<'a> {
    fn ui(self, ui: &mut Ui) -> Response {
        let width = ui.available_width().max(200.0);
        let size = Vec2::new(width, self.height);
        let (rect, mut response) = ui.allocate_exact_size(size, Sense::click_and_drag());

        let pad_left = 32.0;
        let pad_right = 16.0;
        let pad_top = 16.0;
        let pad_bottom = 22.0;

        let graph_rect = Rect::from_min_max(
            rect.min + Vec2::new(pad_left, pad_top),
            rect.max - Vec2::new(pad_right, pad_bottom),
        );

        let min_freq = 20.0f32;
        let max_freq = 20000.0f32;
        let log_min = min_freq.ln();
        let log_max = max_freq.ln();

        let freq_to_x = |f: f32| -> f32 {
            let norm = ((f.max(min_freq).ln() - log_min) / (log_max - log_min)).clamp(0.0, 1.0);
            graph_rect.left() + norm * graph_rect.width()
        };

        let x_to_freq = |x: f32| -> f32 {
            let norm = ((x - graph_rect.left()) / graph_rect.width()).clamp(0.0, 1.0);
            (log_min + norm * (log_max - log_min)).exp()
        };

        let min_db = -36.0f32;
        let max_db = 18.0f32;

        let db_to_y = |db: f32| -> f32 {
            let norm = ((db - min_db) / (max_db - min_db)).clamp(0.0, 1.0);
            graph_rect.bottom() - norm * graph_rect.height()
        };

        let y_to_q = |y: f32| -> f32 {
            let norm = ((graph_rect.bottom() - y) / graph_rect.height()).clamp(0.0, 1.0);
            0.2 + norm * 9.8
        };

        if response.dragged() {
            if let Some(pos) = response.interact_pointer_pos() {
                let new_f = x_to_freq(pos.x).clamp(20.0, 20000.0);
                let new_q = y_to_q(pos.y).clamp(0.2, 10.0);
                *self.cutoff = new_f;
                *self.resonance = new_q;
                response.mark_changed();
            }
        }

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();

            // Background frame
            painter.rect(
                rect,
                CornerRadius::same(6),
                Color32::from_rgb(18, 20, 25),
                Stroke::new(1.0, Color32::from_rgb(38, 44, 54)),
                StrokeKind::Inside,
            );

            // Frequency Grid lines
            let grid_color = Color32::from_rgb(28, 33, 42);
            let text_color = Color32::from_rgb(105, 115, 130);
            let font = egui::FontId::monospace(9.0);

            for &(f, label) in &[
                (20.0, "20"),
                (100.0, "100"),
                (1000.0, "1k"),
                (10000.0, "10k"),
                (20000.0, "20k"),
            ] {
                let x = freq_to_x(f);
                painter.line_segment([Pos2::new(x, graph_rect.top()), Pos2::new(x, graph_rect.bottom())], Stroke::new(1.0, grid_color));
                painter.text(Pos2::new(x, rect.bottom() - 8.0), egui::Align2::CENTER_CENTER, label, font.clone(), text_color);
            }

            // dB Grid lines
            for &db in &[12.0, 0.0, -12.0, -24.0] {
                let y = db_to_y(db);
                painter.line_segment([Pos2::new(graph_rect.left(), y), Pos2::new(graph_rect.right(), y)], Stroke::new(1.0, grid_color));
                let db_label = format!("{:+.0}", db);
                painter.text(Pos2::new(rect.left() + 14.0, y), egui::Align2::RIGHT_CENTER, db_label, font.clone(), text_color);
            }

            let y_zero = db_to_y(0.0);
            painter.line_segment([Pos2::new(graph_rect.left(), y_zero), Pos2::new(graph_rect.right(), y_zero)], Stroke::new(1.0, Color32::from_rgb(45, 52, 65)));

            // Calculate Cytomic SVF Frequency Response Magnitude
            let fc = *self.cutoff;
            let q = *self.resonance;
            let steps = 96;
            let mut points = Vec::with_capacity(steps);

            for i in 0..steps {
                let norm = i as f32 / (steps - 1) as f32;
                let f = (log_min + norm * (log_max - log_min)).exp();
                let w = f / fc;

                let re = 1.0 - w * w;
                let im = w / q;
                let denom_mag_sq = re * re + im * im;

                let mag_sq = match self.mode {
                    FilterMode::LowPass => 1.0 / denom_mag_sq,
                    FilterMode::HighPass => (w * w * w * w) / denom_mag_sq,
                    FilterMode::BandPass => ((w / q) * (w / q)) / denom_mag_sq,
                    FilterMode::Notch => {
                        let num_re = 1.0 - w * w;
                        (num_re * num_re) / denom_mag_sq
                    }
                };

                let mag = mag_sq.sqrt().max(1e-5);
                let db = (20.0 * mag.log10()).clamp(min_db, max_db);
                let x = graph_rect.left() + norm * graph_rect.width();
                let y = db_to_y(db);
                points.push(Pos2::new(x, y));
            }

            // Filled gradient under curve
            let mut fill_points = points.clone();
            fill_points.push(Pos2::new(graph_rect.right(), graph_rect.bottom()));
            fill_points.push(Pos2::new(graph_rect.left(), graph_rect.bottom()));
            painter.add(egui::Shape::convex_polygon(fill_points, self.accent.linear_multiply(0.15), Stroke::NONE));

            // Curve line
            for w in points.windows(2) {
                painter.line_segment([w[0], w[1]], Stroke::new(2.5, self.accent));
            }

            // Crosshair handle
            let cx = freq_to_x(*self.cutoff);
            let cy = db_to_y(match self.mode {
                FilterMode::LowPass | FilterMode::BandPass => (20.0 * q.log10()).clamp(-12.0, 18.0),
                _ => 0.0,
            });

            painter.line_segment([Pos2::new(cx, graph_rect.top()), Pos2::new(cx, graph_rect.bottom())], Stroke::new(1.0, self.accent.linear_multiply(0.5)));
            painter.circle_filled(Pos2::new(cx, cy), 5.0, Color32::WHITE);
            painter.circle_stroke(Pos2::new(cx, cy), 5.0, Stroke::new(2.0, self.accent));

            let status = format!("{:.0} Hz | Q {:.1}", *self.cutoff, *self.resonance);
            painter.text(
                Pos2::new(graph_rect.right() - 8.0, graph_rect.top() + 8.0),
                egui::Align2::RIGHT_TOP,
                status,
                egui::FontId::monospace(10.5),
                self.accent,
            );
        }

        response
    }
}
