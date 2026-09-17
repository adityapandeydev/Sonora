use egui::{Color32, CornerRadius, Pos2, Rect, Response, Sense, Stroke, StrokeKind, Ui, Vec2, Widget};

#[derive(Clone, Debug)]
pub struct AdsrParams {
    pub attack: f32,   // seconds (0.001 .. 2.0)
    pub decay: f32,    // seconds (0.001 .. 2.0)
    pub sustain: f32,  // ratio (0.0 .. 1.0)
    pub release: f32,  // seconds (0.001 .. 3.0)
}

impl Default for AdsrParams {
    fn default() -> Self {
        Self {
            attack: 0.05,
            decay: 0.20,
            sustain: 0.60,
            release: 0.35,
        }
    }
}

pub struct AdsrCanvas<'a> {
    params: &'a mut AdsrParams,
    accent: Color32,
    height: f32,
}

impl<'a> AdsrCanvas<'a> {
    pub fn new(params: &'a mut AdsrParams) -> Self {
        Self {
            params,
            accent: Color32::from_rgb(0, 242, 254),
            height: 120.0,
        }
    }

    pub fn accent(mut self, color: Color32) -> Self {
        self.accent = color;
        self
    }

    pub fn height(mut self, height: f32) -> Self {
        self.height = height;
        self
    }
}

impl<'a> Widget for AdsrCanvas<'a> {
    fn ui(self, ui: &mut Ui) -> Response {
        let width = ui.available_width().max(200.0);
        let size = Vec2::new(width, self.height);
        let (rect, mut response) = ui.allocate_exact_size(size, Sense::click_and_drag());

        let padding = 12.0;
        let graph_rect = Rect::from_min_max(
            rect.min + Vec2::new(padding, padding),
            rect.max - Vec2::new(padding, padding + 16.0),
        );

        let max_time_slice = 3.5f32; // seconds
        let sustain_visual_width = 0.25 * graph_rect.width();
        let total_avail_time_width = graph_rect.width() - sustain_visual_width;

        let time_to_x = |t: f32| -> f32 {
            (t / max_time_slice).clamp(0.0, 1.0) * total_avail_time_width
        };

        let x0 = graph_rect.left();
        let y_bottom = graph_rect.bottom();
        let y_top = graph_rect.top();

        let x_attack = (x0 + time_to_x(self.params.attack)).min(x0 + total_avail_time_width * 0.35);
        let p_attack = Pos2::new(x_attack, y_top);

        let x_decay = (x_attack + time_to_x(self.params.decay)).min(x0 + total_avail_time_width * 0.65);
        let y_sustain = y_bottom - self.params.sustain * (y_bottom - y_top);
        let p_decay = Pos2::new(x_decay, y_sustain);

        let x_sustain_end = x_decay + sustain_visual_width;
        let p_sustain_end = Pos2::new(x_sustain_end, y_sustain);

        let x_release = (x_sustain_end + time_to_x(self.params.release)).min(graph_rect.right());
        let p_release = Pos2::new(x_release, y_bottom);

        // Drag node interaction
        if response.dragged() {
            if let Some(mouse_pos) = response.interact_pointer_pos() {
                let d_att = mouse_pos.distance(p_attack);
                let d_dec = mouse_pos.distance(p_decay);
                let d_rel = mouse_pos.distance(p_release);

                let delta = response.drag_delta();

                if d_att < d_dec && d_att < d_rel {
                    let new_a = (self.params.attack + delta.x * 0.005).clamp(0.002, 1.5);
                    self.params.attack = new_a;
                    response.mark_changed();
                } else if d_dec < d_rel {
                    let new_d = (self.params.decay + delta.x * 0.005).clamp(0.005, 2.0);
                    let new_s = (self.params.sustain - delta.y * 0.01).clamp(0.0, 1.0);
                    self.params.decay = new_d;
                    self.params.sustain = new_s;
                    response.mark_changed();
                } else {
                    let new_r = (self.params.release + delta.x * 0.006).clamp(0.01, 3.0);
                    self.params.release = new_r;
                    response.mark_changed();
                }
            }
        }

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();

            // Background frame
            painter.rect(
                rect,
                CornerRadius::same(6),
                Color32::from_rgb(18, 21, 26),
                Stroke::new(1.0, Color32::from_rgb(36, 42, 52)),
                StrokeKind::Inside,
            );

            // Grid lines
            let grid_color = Color32::from_rgb(26, 31, 39);
            painter.line_segment([Pos2::new(graph_rect.left(), y_top), Pos2::new(graph_rect.right(), y_top)], Stroke::new(1.0, grid_color));
            painter.line_segment([Pos2::new(graph_rect.left(), y_sustain), Pos2::new(graph_rect.right(), y_sustain)], Stroke::new(1.0, grid_color));
            painter.line_segment([Pos2::new(graph_rect.left(), y_bottom), Pos2::new(graph_rect.right(), y_bottom)], Stroke::new(1.0, grid_color));

            // Filled polygon under envelope curve
            let fill_color = self.accent.linear_multiply(0.12);
            let poly_points = vec![
                Pos2::new(x0, y_bottom),
                p_attack,
                p_decay,
                p_sustain_end,
                p_release,
                Pos2::new(p_release.x, y_bottom),
            ];
            painter.add(egui::Shape::convex_polygon(poly_points, fill_color, Stroke::NONE));

            // Envelope line segments
            let line_stroke = Stroke::new(2.5, self.accent);
            painter.line_segment([Pos2::new(x0, y_bottom), p_attack], line_stroke);
            painter.line_segment([p_attack, p_decay], line_stroke);
            painter.line_segment([p_decay, p_sustain_end], Stroke::new(2.5, self.accent.linear_multiply(0.85)));
            painter.line_segment([p_sustain_end, p_release], line_stroke);

            // Control point handles (nodes)
            let node_radius = 4.5;
            let node_color = Color32::WHITE;
            painter.circle_filled(p_attack, node_radius, node_color);
            painter.circle_stroke(p_attack, node_radius, Stroke::new(1.5, self.accent));

            painter.circle_filled(p_decay, node_radius, node_color);
            painter.circle_stroke(p_decay, node_radius, Stroke::new(1.5, self.accent));

            painter.circle_filled(p_release, node_radius, node_color);
            painter.circle_stroke(p_release, node_radius, Stroke::new(1.5, self.accent));

            // Labels and metrics below graph
            let text_color = Color32::from_rgb(140, 150, 165);
            let font = egui::FontId::monospace(9.5);

            let att_str = format!("A: {:.0}ms", self.params.attack * 1000.0);
            painter.text(Pos2::new(x_attack, rect.bottom() - 8.0), egui::Align2::CENTER_CENTER, att_str, font.clone(), text_color);

            let dec_str = format!("D: {:.0}ms", self.params.decay * 1000.0);
            painter.text(Pos2::new(p_decay.x, rect.bottom() - 8.0), egui::Align2::CENTER_CENTER, dec_str, font.clone(), text_color);

            let sus_str = format!("S: {:.0}%", self.params.sustain * 100.0);
            painter.text(Pos2::new((p_decay.x + p_sustain_end.x) * 0.5, rect.bottom() - 8.0), egui::Align2::CENTER_CENTER, sus_str, font.clone(), text_color);

            let rel_str = format!("R: {:.0}ms", self.params.release * 1000.0);
            painter.text(Pos2::new(p_release.x, rect.bottom() - 8.0), egui::Align2::CENTER_CENTER, rel_str, font, text_color);
        }

        response
    }
}
