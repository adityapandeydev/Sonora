use egui::{Color32, Pos2, Response, Sense, Stroke, Ui, Vec2, Widget};
use std::f32::consts::PI;

pub struct RotaryKnob<'a> {
    value: &'a mut f32,
    min: f32,
    max: f32,
    default_val: f32,
    label: &'a str,
    unit: &'a str,
    accent: Color32,
    size: f32,
    is_log: bool,
}

impl<'a> RotaryKnob<'a> {
    pub fn new(value: &'a mut f32, min: f32, max: f32, default_val: f32, label: &'a str) -> Self {
        Self {
            value,
            min,
            max,
            default_val,
            label,
            unit: "",
            accent: Color32::from_rgb(0, 242, 254),
            size: 44.0,
            is_log: false,
        }
    }

    pub fn unit(mut self, unit: &'a str) -> Self {
        self.unit = unit;
        self
    }

    pub fn accent(mut self, color: Color32) -> Self {
        self.accent = color;
        self
    }

    pub fn size(mut self, size: f32) -> Self {
        self.size = size;
        self
    }

    pub fn log(mut self, is_log: bool) -> Self {
        self.is_log = is_log;
        self
    }
}

impl<'a> Widget for RotaryKnob<'a> {
    fn ui(self, ui: &mut Ui) -> Response {
        let total_size = Vec2::new(self.size + 16.0, self.size + 32.0);
        let (rect, mut response) = ui.allocate_exact_size(total_size, Sense::click_and_drag());

        // Normalized fraction 0.0 .. 1.0
        let mut norm = if self.is_log {
            ((self.value.max(self.min) / self.min).ln()) / ((self.max / self.min).ln())
        } else {
            (*self.value - self.min) / (self.max - self.min)
        }.clamp(0.0, 1.0);

        // Interaction: Drag up/down
        if response.dragged() {
            let delta = -response.drag_delta().y * 0.006;
            let speed_mod = if ui.input(|i| i.modifiers.shift) { 0.15 } else { 1.0 };
            norm = (norm + delta * speed_mod).clamp(0.0, 1.0);

            *self.value = if self.is_log {
                self.min * ((self.max / self.min).ln() * norm).exp()
            } else {
                self.min + norm * (self.max - self.min)
            };
            response.mark_changed();
        }

        // Double click reset
        if response.double_clicked() {
            *self.value = self.default_val;
            norm = if self.is_log {
                ((self.value.max(self.min) / self.min).ln()) / ((self.max / self.min).ln())
            } else {
                (*self.value - self.min) / (self.max - self.min)
            }.clamp(0.0, 1.0);
            response.mark_changed();
        }

        // Paint Knob
        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let center = Pos2::new(rect.center().x, rect.top() + self.size * 0.5 + 4.0);
            let radius = self.size * 0.5;

            let start_angle = 0.75 * PI; // 135 deg (bottom left)
            let end_angle = 2.25 * PI;   // 405 deg (bottom right)
            let current_angle = start_angle + norm * (end_angle - start_angle);

            // 1. Background Track Arc
            let track_color = Color32::from_rgb(32, 38, 46);
            let arc_steps = 32;
            for i in 0..arc_steps {
                let a1 = start_angle + (i as f32 / arc_steps as f32) * (end_angle - start_angle);
                let a2 = start_angle + ((i + 1) as f32 / arc_steps as f32) * (end_angle - start_angle);
                let p1 = center + Vec2::new(a1.cos(), a1.sin()) * (radius + 2.0);
                let p2 = center + Vec2::new(a2.cos(), a2.sin()) * (radius + 2.0);
                painter.line_segment([p1, p2], Stroke::new(3.5, track_color));
            }

            // 2. Active Glowing Value Arc
            let active_steps = ((arc_steps as f32) * norm).ceil() as usize;
            for i in 0..active_steps {
                let f1 = (i as f32 / arc_steps as f32).min(norm);
                let f2 = ((i + 1) as f32 / arc_steps as f32).min(norm);
                let a1 = start_angle + f1 * (end_angle - start_angle);
                let a2 = start_angle + f2 * (end_angle - start_angle);
                let p1 = center + Vec2::new(a1.cos(), a1.sin()) * (radius + 2.0);
                let p2 = center + Vec2::new(a2.cos(), a2.sin()) * (radius + 2.0);
                painter.line_segment([p1, p2], Stroke::new(3.5, self.accent));
            }

            // 3. Central Dial Body (metallic rim + dark cap)
            let dial_radius = radius - 3.0;
            let body_color = if response.hovered() || response.dragged() {
                Color32::from_rgb(42, 48, 58)
            } else {
                Color32::from_rgb(26, 30, 36)
            };
            painter.circle_filled(center, dial_radius, body_color);
            painter.circle_stroke(
                center,
                dial_radius,
                Stroke::new(1.5, if response.dragged() { self.accent } else { Color32::from_rgb(50, 58, 70) }),
            );

            // 4. Pointer Indicator Needle
            let pointer_start = center + Vec2::new(current_angle.cos(), current_angle.sin()) * (dial_radius * 0.35);
            let pointer_end = center + Vec2::new(current_angle.cos(), current_angle.sin()) * (dial_radius * 0.90);
            painter.line_segment(
                [pointer_start, pointer_end],
                Stroke::new(2.5, if response.hovered() { Color32::WHITE } else { self.accent }),
            );

            // 5. Label
            let label_pos = Pos2::new(center.x, rect.bottom() - 18.0);
            painter.text(
                label_pos,
                egui::Align2::CENTER_CENTER,
                self.label,
                egui::FontId::proportional(11.0),
                Color32::from_rgb(160, 168, 180),
            );

            // 6. Value string
            let val_str = if *self.value >= 1000.0 && self.unit == "Hz" {
                format!("{:.1}k {}", *self.value / 1000.0, self.unit)
            } else if *self.value >= 10.0 {
                format!("{:.0} {}", *self.value, self.unit)
            } else if *self.value >= 1.0 {
                format!("{:.1} {}", *self.value, self.unit)
            } else {
                format!("{:.2} {}", *self.value, self.unit)
            };

            let val_pos = Pos2::new(center.x, rect.bottom() - 6.0);
            painter.text(
                val_pos,
                egui::Align2::CENTER_CENTER,
                val_str,
                egui::FontId::monospace(9.5),
                if response.hovered() { self.accent } else { Color32::from_rgb(110, 120, 135) },
            );
        }

        response
    }
}
