#![feature(iter_intersperse)]

use std::collections::HashMap;
use std::io::Read;

use nom::bytes::streaming::tag;
use nom::character::complete::digit1;
use nom::multi::separated_list1;
use nom::IResult;

fn main() {
    let mut buffer = vec![];
    std::io::stdin().read_to_end(&mut buffer).unwrap();
    let input = String::from_utf8(buffer).unwrap();
    let result_1 = part1(&input);

    println!("part1: {}", result_1);

    let result_2 = part2(&input);
    println!("part2: {}", result_2);
}

#[derive(Debug, Hash, PartialEq, PartialOrd, Ord, Eq, Clone, Copy)]
struct Point {
    x: usize,
    y: usize,
}

impl From<(usize, usize)> for Point {
    fn from((x, y): (usize, usize)) -> Self {
        Self { x, y }
    }
}

impl Point {
    fn down(&self) -> Self {
        (self.x, self.y + 1).into()
    }
    fn left_down(&self) -> Self {
        (self.x - 1, self.y + 1).into()
    }
    fn right_down(&self) -> Self {
        (self.x + 1, self.y + 1).into()
    }
}

struct Path(Vec<Point>);

struct Bound {
    max: Point,
    min: Point,
}

struct Boundaries {
    bound: Option<Bound>,
}

struct BoundaryIterator<'a> {
    boundaries: &'a Boundaries,
    current: Option<Point>,
    done: bool,
}

impl<'a> Iterator for BoundaryIterator<'a> {
    type Item = Point;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        if let Some(bound) = &self.boundaries.bound {
            if let Some(c) = self.current {
                let next_point: Option<Point> = if c.x == bound.max.x {
                    if c.y == bound.max.y {
                        self.done = true;
                        None
                    } else {
                        Some((bound.min.x, c.y + 1).into())
                    }
                } else {
                    Some((c.x + 1, c.y).into())
                };
                self.current = next_point;
            } else {
                self.current = Some(bound.min);
            }
            self.current
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test_iter {
    use crate::Boundaries;

    #[test]
    fn iter() {
        let point1 = (3, 3).into();
        let point2 = (5, 2).into();

        let mut boundaries = Boundaries { bound: None };
        let v = boundaries.enumerate().collect::<Vec<_>>();
        assert_eq!(v, vec![]);

        boundaries.grow_to_include(&point1);
        boundaries.grow_to_include(&point2);
        let v = boundaries.enumerate().collect::<Vec<_>>();

        assert_eq!(
            v,
            vec![
                (3, 2).into(),
                (4, 2).into(),
                (5, 2).into(),
                (3, 3).into(),
                (4, 3).into(),
                (5, 3).into()
            ]
        )
    }
}

impl Boundaries {
    fn in_bound(&self, point: &Point) -> bool {
        if let Some(bound) = &self.bound {
            !(point.x < bound.min.x
                || point.y < bound.min.y
                || point.x > bound.max.x
                || point.y > bound.max.y)
        } else {
            false
        }
    }

    fn grow_to_include(&mut self, point: &Point) {
        if let Some(bound) = &mut self.bound {
            bound.max.x = bound.max.x.max(point.x);
            bound.min.x = bound.min.x.min(point.x);
            bound.max.y = bound.max.y.max(point.y);
            bound.min.y = bound.min.y.min(point.y);
        } else {
            self.bound = Some(Bound {
                max: *point,
                min: *point,
            })
        }
    }

    fn enumerate(&self) -> BoundaryIterator<'_> {
        BoundaryIterator {
            boundaries: self,
            current: None,
            done: false,
        }
    }

    fn x_len(&self) -> usize {
        if let Some(b) = &self.bound {
            b.max.x - b.min.x + 1
        } else {
            0
        }
    }
}

#[cfg(test)]
mod test_boundaries {
    use crate::Boundaries;

    #[test]
    fn bound() {
        let mut b = Boundaries { bound: None };
        let point = (2, 7).into();
        let point2 = (4, 3).into();
        let point_mid = (3, 5).into();

        assert!(!b.in_bound(&point));
        assert!(!b.in_bound(&point2));
        assert!(!b.in_bound(&point_mid));

        b.grow_to_include(&point);
        assert!(b.in_bound(&point));
        assert!(!b.in_bound(&point2));
        assert!(!b.in_bound(&point_mid));

        b.grow_to_include(&point2);
        assert!(b.in_bound(&point));
        assert!(b.in_bound(&point2));
        assert!(b.in_bound(&point_mid));
    }
}

#[derive(Clone, Copy)]
enum Element {
    Void,
    Air,
    Rock,
    Sand,
}

struct Grid {
    map: HashMap<Point, Element>,
    boundaries: Boundaries,
}

impl ToString for Grid {
    fn to_string(&self) -> String {
        self.boundaries
            .enumerate()
            .map(|point| match self.get(&point) {
                Element::Void | Element::Air => '.',
                Element::Rock => '#',
                Element::Sand => 'o',
            })
            .collect::<Vec<_>>()
            .chunks(self.boundaries.x_len())
            .intersperse(&['\n'])
            .flatten()
            .collect::<String>()
    }
}

impl Grid {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
            boundaries: Boundaries { bound: None },
        }
    }

    fn get(&self, point: &Point) -> Element {
        match self.map.get(point) {
            Some(element) => *element,
            None => {
                if self.boundaries.in_bound(point) {
                    Element::Air
                } else {
                    Element::Void
                }
            }
        }
    }

    fn add(&mut self, point: Point, element: Element) -> Option<Element> {
        self.map.insert(point, element)
    }

    fn add_path(&mut self, path: Path, element: Element) {
        path.0
            .into_iter()
            .fold(None, |cursor: Option<Point>, point| {
                self.boundaries.grow_to_include(&point);
                if let Some(c) = cursor {
                    if c.y == point.y {
                        let min = c.x.min(point.x);
                        let max = c.x.max(point.x);
                        for px in min..=max {
                            self.add((px, point.y).into(), element);
                        }
                    }
                    if c.x == point.x {
                        let min = c.y.min(point.y);
                        let max = c.y.max(point.y);
                        for py in min..=max {
                            self.add((point.x, py).into(), element);
                        }
                    }
                }
                Some(point)
            });
    }
}

fn drop_sand(from: Point, grid: &mut Grid) -> Option<Point> {
    if let Some(bound) = &grid.boundaries.bound {
        if from.y > bound.max.y {
            return None;
        }
    }
    match grid.get(&from) {
        Element::Rock | Element::Sand => None,
        Element::Void | Element::Air => Some(drop_sand(from.down(), grid).unwrap_or_else(|| {
            drop_sand(from.left_down(), grid)
                .unwrap_or_else(|| drop_sand(from.right_down(), grid).unwrap_or(from))
        })),
    }
}

fn parse_path(input: &str) -> IResult<&str, Path> {
    nom::combinator::map(separated_list1(tag(" -> "), parse_point), Path)(input)
}

fn parse_point(input: &str) -> IResult<&str, Point> {
    nom::combinator::map(
        nom::sequence::tuple((digit1, tag(","), digit1)),
        |(x, _, y): (&str, _, &str)| (x.parse().unwrap(), y.parse().unwrap()).into(),
    )(input)
}

fn part1(input: &str) -> i64 {
    // let paths: Vec<Path> = vec![
    //     Path(vec![(498, 4).into(), (498, 6).into(), (496, 6).into()]),
    //     Path(vec![
    //         (503, 4).into(),
    //         (502, 4).into(),
    //         (502, 9).into(),
    //         (494, 9).into(),
    //     ]),
    // ];

    let (_, paths) = separated_list1(tag("\n"), parse_path)(input).expect("paths parse correctly");

    let mut grid = Grid::new();

    for path in paths {
        grid.add_path(path, Element::Rock);
    }

    let thing = drop_sand((500,0).into(), &mut grid);

    println!("{}", grid.to_string());

    println!("thing: {:?}", thing);

    0
}

fn part2(input: &str) -> i64 {
    0
}

#[cfg(test)]
mod test {

    const INPUT: &str = include_str!("../input-example.txt");
    #[test]
    fn part1() {
        assert_eq!(super::part1(INPUT), 0);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(INPUT), 0);
    }
}
