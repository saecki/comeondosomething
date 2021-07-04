use gtk4::prelude::*;
use gtk4::Application;
use gtk4::ApplicationWindow;
use gtk4::Orientation;
use gtk4::Text;
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

    let output = Text::new();
    let input = TextView::new();

    let container = gtk4::Box::builder()
        .orientation(Orientation::Vertical)
        .build();
    container.append(&output);
    container.append(&input);
    window.set_child(Some(&container));

    let buffer = input.buffer();
    buffer.connect_changed(move |t| {
        let gs = t.slice(&t.start_iter(), &t.end_iter(), true);
        let s = gs.as_str();

        match cods::calc(s) {
            Ok((v, warnings)) => {
                for _w in warnings.iter() {
                    // string.push_str(&w.display(s).to_string());
                }

                output.set_text(&v.to_string());
            }
            Err(errors) => {
                for _e in errors.iter() {
                    // string.push_str(&e.display(s).to_string());
                }

                output.set_text("");
            }
        }
    });

    window.present();
}
