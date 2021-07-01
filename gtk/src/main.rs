use gtk4::prelude::*;
use gtk4::Application;
use gtk4::ApplicationWindow;
use gtk4::TextView;

fn main() {
    let app = Application::new(Some("saecki.cods"), Default::default());
    app.connect_activate(build_ui);

    app.run();
}

fn build_ui(app: &Application) {
    let window = ApplicationWindow::builder()
        .application(app)
        .title("cods")
        .default_width(800)
        .default_height(600)
        .build();

    let text_view = TextView::builder()
        .margin_top(12)
        .margin_end(12)
        .margin_bottom(12)
        .margin_start(12)
        .build();

    let buffer = text_view.buffer();

    buffr.connect_changed(move || {
        println!("changed");
    });

    window.set_child(Some(&text_view));
    window.present();
}
