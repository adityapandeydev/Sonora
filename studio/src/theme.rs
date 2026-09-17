use egui::{Color32, CornerRadius, Margin, Stroke, Vec2, Visuals};

pub struct StudioTheme;

#[allow(dead_code)]
impl StudioTheme {
    // Chassis & Backgrounds
    pub const BG_OBSIDIAN: Color32 = Color32::from_rgb(14, 16, 19);
    pub const BG_PANEL: Color32    = Color32::from_rgb(22, 25, 30);
    pub const BG_CARD: Color32     = Color32::from_rgb(28, 32, 38);
    pub const BG_CARD_HOVER: Color32 = Color32::from_rgb(36, 41, 49);
    pub const BG_INPUT: Color32    = Color32::from_rgb(18, 20, 24);

    // Accent Colors (Hardware Synth Glow)
    pub const ACCENT_CYAN: Color32   = Color32::from_rgb(0, 242, 254);   // Leads / Oscillators
    pub const ACCENT_AMBER: Color32  = Color32::from_rgb(255, 159, 28);  // Filter / Resonance
    pub const ACCENT_VIOLET: Color32 = Color32::from_rgb(157, 78, 221);  // Effects / Reverb
    pub const ACCENT_GREEN: Color32  = Color32::from_rgb(46, 196, 182);  // Procedural / Transport
    pub const ACCENT_RED: Color32    = Color32::from_rgb(230, 57, 70);   // Errors / Limiter
    pub const ACCENT_BLUE: Color32   = Color32::from_rgb(79, 128, 255);  // Navigation / Tabs

    // Text & Muted
    pub const TEXT_PRIMARY: Color32   = Color32::from_rgb(243, 244, 246);
    pub const TEXT_SECONDARY: Color32 = Color32::from_rgb(160, 168, 180);
    pub const TEXT_MUTED: Color32     = Color32::from_rgb(95, 105, 120);

    // Borders & Dividers
    pub const BORDER_DARK: Color32  = Color32::from_rgb(40, 46, 56);
    pub const BORDER_LIGHT: Color32 = Color32::from_rgb(55, 62, 75);

    /// Apply dark studio visuals to egui context
    pub fn apply(ctx: &egui::Context) {
        let mut visuals = Visuals::dark();
        visuals.panel_fill = Self::BG_PANEL;
        visuals.window_fill = Self::BG_OBSIDIAN;
        visuals.faint_bg_color = Self::BG_CARD;
        visuals.extreme_bg_color = Self::BG_INPUT;

        visuals.widgets.noninteractive.bg_fill = Self::BG_CARD;
        visuals.widgets.noninteractive.bg_stroke = Stroke::new(1.0, Self::BORDER_DARK);
        visuals.widgets.noninteractive.fg_stroke = Stroke::new(1.0, Self::TEXT_PRIMARY);
        visuals.widgets.noninteractive.corner_radius = CornerRadius::same(6);

        visuals.widgets.inactive.bg_fill = Self::BG_CARD;
        visuals.widgets.inactive.bg_stroke = Stroke::new(1.0, Self::BORDER_DARK);
        visuals.widgets.inactive.fg_stroke = Stroke::new(1.0, Self::TEXT_PRIMARY);
        visuals.widgets.inactive.corner_radius = CornerRadius::same(6);

        visuals.widgets.hovered.bg_fill = Self::BG_CARD_HOVER;
        visuals.widgets.hovered.bg_stroke = Stroke::new(1.5, Self::ACCENT_CYAN);
        visuals.widgets.hovered.fg_stroke = Stroke::new(1.0, Color32::WHITE);
        visuals.widgets.hovered.corner_radius = CornerRadius::same(6);

        visuals.widgets.active.bg_fill = Self::ACCENT_CYAN.linear_multiply(0.2);
        visuals.widgets.active.bg_stroke = Stroke::new(1.5, Self::ACCENT_CYAN);
        visuals.widgets.active.fg_stroke = Stroke::new(1.0, Self::ACCENT_CYAN);
        visuals.widgets.active.corner_radius = CornerRadius::same(6);

        visuals.selection.bg_fill = Self::ACCENT_CYAN.linear_multiply(0.35);
        visuals.selection.stroke = Stroke::new(1.0, Self::ACCENT_CYAN);

        ctx.set_visuals(visuals);

        let mut style = (*ctx.style()).clone();
        style.spacing.item_spacing = Vec2::new(8.0, 6.0);
        style.spacing.window_margin = Margin::same(12);
        ctx.set_style(style);
    }
}
