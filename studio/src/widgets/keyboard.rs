use egui::{Color32, CornerRadius, Pos2, Rect, Response, Sense, Stroke, StrokeKind, Ui, Vec2, Widget};

pub struct VirtualKeyboard<'a> {
    pressed_note: &'a mut Option<String>,
    start_octave: usize,
    height: f32,
}

impl<'a> VirtualKeyboard<'a> {
    pub fn new(pressed_note: &'a mut Option<String>) -> Self {
        Self {
            pressed_note,
            start_octave: 3,
            height: 90.0,
        }
    }

    pub fn start_octave(mut self, oct: usize) -> Self {
        self.start_octave = oct;
        self
    }

    pub fn height(mut self, h: f32) -> Self {
        self.height = h;
        self
    }
}

impl<'a> Widget for VirtualKeyboard<'a> {
    fn ui(self, ui: &mut Ui) -> Response {
        let width = ui.available_width().max(300.0);
        let size = Vec2::new(width, self.height);
        let (rect, mut response) = ui.allocate_exact_size(size, Sense::click_and_drag());

        let num_white_keys = 14;
        let white_key_width = rect.width() / (num_white_keys as f32);
        let white_key_height = rect.height();
        let black_key_width = white_key_width * 0.62;
        let black_key_height = white_key_height * 0.60;

        let mut clicked_pitch: Option<String> = None;

        let white_notes = [
            ("C", 0), ("D", 0), ("E", 0), ("F", 0), ("G", 0), ("A", 0), ("B", 0),
            ("C", 1), ("D", 1), ("E", 1), ("F", 1), ("G", 1), ("A", 1), ("B", 1),
        ];

        let black_notes = [
            (0, "C#", 0),
            (1, "D#", 0),
            (3, "F#", 0),
            (4, "G#", 0),
            (5, "A#", 0),
            (7, "C#", 1),
            (8, "D#", 1),
            (10, "F#", 1),
            (11, "G#", 1),
            (12, "A#", 1),
        ];

        let pointer_pos = response.interact_pointer_pos();
        let is_interacting = response.clicked() || response.dragged();

        if is_interacting {
            if let Some(pos) = pointer_pos {
                for &(w_idx, name, oct_off) in &black_notes {
                    let bx = rect.left() + (w_idx as f32 + 1.0) * white_key_width - (black_key_width * 0.5);
                    let b_rect = Rect::from_min_size(Pos2::new(bx, rect.top()), Vec2::new(black_key_width, black_key_height));
                    if b_rect.contains(pos) {
                        let full_pitch = format!("{}{}", name, self.start_octave + oct_off);
                        clicked_pitch = Some(full_pitch);
                        break;
                    }
                }

                if clicked_pitch.is_none() && rect.contains(pos) {
                    let w_idx = (((pos.x - rect.left()) / white_key_width).floor() as usize).min(num_white_keys - 1);
                    let (name, oct_off) = white_notes[w_idx];
                    let full_pitch = format!("{}{}", name, self.start_octave + oct_off);
                    clicked_pitch = Some(full_pitch);
                }
            }
        }

        if let Some(ref p) = clicked_pitch {
            *self.pressed_note = Some(p.clone());
            response.mark_changed();
        }

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();

            painter.rect_filled(rect, CornerRadius::same(4), Color32::from_rgb(15, 17, 21));

            // White Keys
            for (i, &(name, oct_off)) in white_notes.iter().enumerate() {
                let kx = rect.left() + (i as f32) * white_key_width;
                let k_rect = Rect::from_min_size(Pos2::new(kx, rect.top()), Vec2::new(white_key_width - 1.0, white_key_height));

                let full_pitch = format!("{}{}", name, self.start_octave + oct_off);
                let is_pressed = self.pressed_note.as_ref() == Some(&full_pitch);

                let fill_color = if is_pressed {
                    Color32::from_rgb(0, 242, 254)
                } else {
                    Color32::from_rgb(228, 232, 240)
                };

                painter.rect(
                    k_rect,
                    CornerRadius::same(3),
                    fill_color,
                    Stroke::new(1.0, Color32::from_rgb(40, 46, 56)),
                    StrokeKind::Inside,
                );

                if name == "C" {
                    let label = format!("C{}", self.start_octave + oct_off);
                    painter.text(
                        Pos2::new(k_rect.center().x, k_rect.bottom() - 10.0),
                        egui::Align2::CENTER_CENTER,
                        label,
                        egui::FontId::monospace(9.0),
                        Color32::from_rgb(90, 100, 115),
                    );
                }
            }

            // Black Keys
            for &(w_idx, name, oct_off) in &black_notes {
                let bx = rect.left() + (w_idx as f32 + 1.0) * white_key_width - (black_key_width * 0.5);
                let b_rect = Rect::from_min_size(Pos2::new(bx, rect.top()), Vec2::new(black_key_width, black_key_height));

                let full_pitch = format!("{}{}", name, self.start_octave + oct_off);
                let is_pressed = self.pressed_note.as_ref() == Some(&full_pitch);

                let fill_color = if is_pressed {
                    Color32::from_rgb(0, 242, 254)
                } else {
                    Color32::from_rgb(32, 36, 44)
                };

                painter.rect(
                    b_rect,
                    CornerRadius::same(2),
                    fill_color,
                    Stroke::new(1.0, Color32::from_rgb(18, 20, 26)),
                    StrokeKind::Inside,
                );
            }
        }

        response
    }
}
