use std::time::{Instant, Duration};

fn main() {
    draw_circ();
}

fn draw_circ() {
    let mut frame: Vec<Vec<i32>> = vec![];
    for i in 0..100 {
        let mut temp = vec![];
        for j in 0..100 {
            let d = get_dist(i, j, 50, 50);
            if d < 10 {
                temp.push(1);
            }
            else {
                temp.push(0);
            }
        }

        let t = Instant::now();
        frame.push(temp);
        println!("time: {:?}", t.elapsed());
    }
    draw(frame);
}

fn draw(frame: Vec<Vec<i32>>) {
    for i in 0..frame.len() {
        for j in 0..frame[0].len() {
            print!("{}", frame[i][j]);
        }
        println!();
    }
}

fn get_dist(x1: i32, y1: i32, x2: i32, y2: i32) -> i32 {
    return ((((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)) as f32).sqrt()) as i32;
}