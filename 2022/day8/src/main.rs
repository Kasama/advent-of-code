use std::io::Read;
use std::ops::{Div, Rem};

fn main() {
    let mut buffer = vec![];
    std::io::stdin().read_to_end(&mut buffer).unwrap();
    let input = String::from_utf8(buffer).unwrap();
    let result_1 = part1(&input);

    println!("part1: {}", result_1);

    let result_2 = part2(&input);
    println!("part2: {}", result_2);
}

#[derive(Debug)]
struct Grid<T> {
    line_size: usize,
    grid: Vec<T>,
}

type Canopy = Grid<usize>;

type Point = (usize, usize);

enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn vector(&self) -> (i32, i32) {
        match self {
            Direction::Up => (0, -1),
            Direction::Down => (0, 1),
            Direction::Left => (-1, 0),
            Direction::Right => (1, 0),
        }
    }
}

impl Canopy {
    fn parse_canopy(input: &str) -> Self {
        Self {
            line_size: input
                .lines()
                .map(|line| line.len())
                .max()
                .expect("some canopy size"),
            grid: input
                .chars()
                .filter_map(|c| format!("{}", c).parse::<usize>().ok())
                .collect::<Vec<_>>(),
        }
    }

    fn is_tree_visible(&self, point: Point) -> bool {
        if self.is_border(point) {
            return true;
        }

        [
            Direction::Up,
            Direction::Down,
            Direction::Left,
            Direction::Right,
        ]
        .map(|d| self.is_tree_visible_from(point, d))
        .iter()
        .any(|a| *a)
    }

    fn is_tree_visible_from(&self, point: Point, direction: Direction) -> bool {
        let direction = direction.vector();

        let opt_tree_height = self.index(point);
        if opt_tree_height.is_none() {
            return false;
        }
        let tree_height = opt_tree_height.unwrap();

        let mut current_point = self.add(point, direction);

        while let Some((x, y)) = current_point {
            if let Some(current_height) = self.index((x, y)) {
                // if there's a tree higher than the target one
                // in that direction that tree is hidden.
                if current_height >= tree_height {
                    return false;
                }
            }

            current_point = self.add((x, y), direction);
        }

        true
    }

    fn viewing_distance(&self, point: Point, direction: Direction) -> usize {
        if self.is_border(point) {
            return 0;
        }

        let direction = direction.vector();

        let opt_tree_height = self.index(point);
        if opt_tree_height.is_none() {
            return 0;
        }
        let tree_height = opt_tree_height.unwrap();

        let mut current_point = self.add(point, direction);

        let mut distance = 1usize;
        while let Some((x, y)) = current_point {
            if let Some(current_height) = self.index((x, y)) {
                if current_height >= tree_height || self.is_border((x, y)) {
                    return distance;
                }
            }

            distance += 1;
            current_point = self.add((x, y), direction);
        }

        distance
    }

    fn scenic_score(&self, point: Point) -> usize {
        [
            Direction::Up,
            Direction::Down,
            Direction::Left,
            Direction::Right,
        ]
        .map(|d| {
            let (x, y) = point;
            self.viewing_distance((x, y), d)
        })
        .iter()
        .product()
    }

    fn visible_trees(&self) -> Vec<usize> {
        self.grid
            .iter()
            .enumerate()
            .filter(|(i, _)| self.is_tree_visible(self.grid_from(*i)))
            .map(|(_, t)| *t)
            .collect::<Vec<_>>()
    }
}

impl<T: Copy> Grid<T> {
    fn grid_from(&self, v: usize) -> Point {
        let x = v.rem(self.line_size);
        let y = v.div(self.line_size);

        (x, y)
    }

    fn grid_into(&self, (x, y): Point) -> usize {
        y * self.line_size + x
    }

    fn is_border(&self, (x, y): Point) -> bool {
        x == 0 || y == 0 || x == self.line_size - 1 || y == self.line_size - 1
    }

    fn add(&self, (x, y): Point, (add_x, add_y): (i32, i32)) -> Option<Point> {
        if self.is_border((x, y)) {
            return None;
        }

        let next_x: i32 = (x as i32) + add_x;
        let next_y: i32 = (y as i32) + add_y;

        Some((next_x as usize, next_y as usize))
    }

    fn index(&self, (x, y): Point) -> Option<T> {
        if x >= self.line_size || y >= self.line_size {
            return None;
        }

        self.grid.get(self.grid_into((x, y))).copied()
    }

    fn map<F: FnMut((usize, &T)) -> I, I: Copy>(&self, f: F) -> Grid<I> {
        Grid {
            line_size: self.line_size,
            grid: self.grid.iter().enumerate().map(f).collect::<Vec<_>>(),
        }
    }
}
#[cfg(test)]
mod canopy_test {
    use crate::{Direction, Grid};

    #[test]
    fn test_grid() {
        let canopy: Grid<usize> = Grid {
            line_size: 5,
            grid: vec![],
        };

        //   y
        // x 0 01234
        //   1 01234
        //   2 01234
        //   3 01234
        //   4 01234
        assert_eq!(canopy.grid_from(5), (0, 1));
        assert_eq!(canopy.grid_into((0, 1)), 5);

        assert_eq!(canopy.grid_from(8), (3, 1));
        assert_eq!(canopy.grid_into((3, 1)), 8);

        assert_eq!(canopy.grid_from(6), (1, 1));
        assert_eq!(canopy.grid_into((1, 1)), 6);

        assert_eq!(canopy.grid_from(24), (4, 4));
        assert_eq!(canopy.grid_into((4, 4)), 24);
    }

    #[test]
    fn test_visibility() {
        let canopy: Grid<usize> = Grid {
            line_size: 5,
            grid: vec![
                3, 0, 3, 7, 3, 2, 5, 5, 1, 2, 6, 5, 3, 3, 2, 3, 3, 5, 4, 9, 3, 5, 3, 9, 0,
            ],
        };

        assert!(canopy.is_tree_visible_from((1, 1), Direction::Up));
        assert!(canopy.is_tree_visible_from((1, 1), Direction::Left));
        assert!(!canopy.is_tree_visible_from((1, 1), Direction::Right));
        assert!(!canopy.is_tree_visible_from((1, 1), Direction::Down));

        assert!(!canopy.is_tree_visible_from((2, 3), Direction::Up));
        assert!(canopy.is_tree_visible_from((2, 3), Direction::Left));
        assert!(!canopy.is_tree_visible_from((2, 3), Direction::Right));
        assert!(canopy.is_tree_visible_from((2, 3), Direction::Down));

        assert!(canopy.is_tree_visible((1, 1)));
        assert!(!canopy.is_tree_visible((2, 2)));
        assert!(!canopy.is_tree_visible((1, 3)));

        assert!(canopy.is_tree_visible((0, 3)));
        assert!(canopy.is_tree_visible((1, 4)));
        assert!(canopy.is_tree_visible((4, 2)));
        assert!(canopy.is_tree_visible((2, 0)));
    }
}

fn part1(input: &str) -> usize {
    let grid = Canopy::parse_canopy(input);

    grid.visible_trees().len()
}

fn part2(input: &str) -> usize {
    let grid = Canopy::parse_canopy(input);

    let new_grid = grid.map(|(i, _)| {
        let point = grid.grid_from(i);
        grid.scenic_score(point)
    });

    new_grid
        .grid
        .into_iter()
        .fold(0, |max, v| if max > v { max } else { v })
}

#[cfg(test)]
mod test {

    const INPUT: &str = include_str!("../input-example.txt");
    #[test]
    fn part1() {
        assert_eq!(super::part1(INPUT), 21);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(INPUT), 8);
    }
}
