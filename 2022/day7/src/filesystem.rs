use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq)]
pub enum CDDestination {
    Root,
    Parent,
    Child(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    LS,
    CD(CDDestination),
}

#[derive(Debug, PartialEq, Eq)]
pub enum CommandWithOutput {
    LS(Vec<OutputDirectoryEntity>),
    CD(CDDestination),
}

#[derive(Debug, PartialEq, Eq)]
pub struct File {
    filename: String,
    size: i64,
}

#[derive(Eq)]
pub struct Directory {
    pub name: String,
    pub parent: Option<Rc<RefCell<Directory>>>,
    pub entities: Vec<Rc<RefCell<DirectoryEntity>>>,
}

impl PartialEq for Directory {
    fn eq(&self, other: &Self) -> bool {
        if !(self.name == other.name && self.entities == other.entities) {
            return false;
        };

        if self.parent.is_none() && self.parent.is_none() {
            return true;
        }

        if let Some(p1) = &self.parent {
            if let Some(p2) = &other.parent {
                let p1_name = p1.borrow().name.clone();
                let p2_name = p2.borrow().name.clone();
                return p1_name == p2_name;
            }
        }

        false
    }
}

impl std::fmt::Debug for Directory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Directory")
            .field("name", &self.name)
            .field(
                "parent",
                &self.parent.as_ref().map(|d| d.borrow().name.clone()),
            )
            .field("entities", &self.entities)
            .finish()
    }
}

impl Directory {
    pub fn total_size(&self) -> i64 {
        self.entities.iter().fold(0, |acc, entity| {
            acc + match &*entity.borrow() {
                DirectoryEntity::File(f) => f.borrow().size,
                DirectoryEntity::Dir(d) => d.borrow().total_size(),
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum DirectoryEntity {
    File(Rc<RefCell<File>>),
    Dir(Rc<RefCell<Directory>>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum OutputDirectoryEntity {
    File((i64, String)),
    Dir(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Filesystem {
    pub current_dir: Rc<RefCell<Directory>>,
    pub root: Rc<RefCell<Directory>>,
}

impl Filesystem {
    pub fn empty() -> Self {
        let root = wrap(Directory {
            name: "/".to_owned(),
            parent: None,
            entities: vec![],
        });

        Self {
            current_dir: root.clone(),
            root,
        }
    }
    pub fn build<I: Iterator<Item = CommandWithOutput>>(commands: I) -> Self {
        commands.fold(Self::empty(), |mut fs, cmd| match cmd {
            CommandWithOutput::LS(entities) => {
                let a = entities
                    .iter()
                    .map(|e| match e {
                        OutputDirectoryEntity::File((size, name)) => {
                            DirectoryEntity::File(wrap(File {
                                filename: name.clone(),
                                size: *size,
                            }))
                        }
                        OutputDirectoryEntity::Dir(d) => DirectoryEntity::Dir(wrap(Directory {
                            name: d.clone(),
                            parent: Some(fs.current_dir.clone()),
                            entities: vec![],
                        })),
                    })
                    .map(wrap)
                    .collect::<Vec<_>>();
                fs.current_dir.borrow_mut().entities = a;
                fs
            }
            CommandWithOutput::CD(dir) => match dir {
                CDDestination::Root => {
                    fs.current_dir = fs.root.clone();
                    fs
                }
                CDDestination::Parent => {
                    let opt_parent = { fs.current_dir.borrow().parent.clone() };
                    if let Some(parent) = opt_parent {
                        fs.current_dir = parent;
                    };
                    fs
                }
                CDDestination::Child(dir) => {
                    let opt_child = {
                        fs.current_dir.borrow().entities.iter().find_map(|de| {
                            let d = (*de).clone();

                            let name = {
                                match &*d.borrow() {
                                    DirectoryEntity::File(_) => None,
                                    DirectoryEntity::Dir(dir) => Some(dir.borrow().name.clone()),
                                }
                            };

                            if let Some(name) = name {
                                if name == dir {
                                    Some(d)
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                    };
                    if let Some(child) = opt_child {
                        if let DirectoryEntity::Dir(dir) = &*child.borrow() {
                            fs.current_dir = dir.clone()
                        }
                    }
                    fs
                }
            },
        })
    }
}

fn wrap<T>(v: T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(v))
}

#[cfg(test)]
mod test {

    use crate::filesystem::wrap;

    use super::{
        CommandWithOutput, Directory, DirectoryEntity, File, Filesystem, OutputDirectoryEntity,
    };

    fn example_fs() -> Filesystem {
        let root = wrap(Directory {
            name: "/".to_owned(),
            parent: None,
            entities: vec![wrap(DirectoryEntity::File(wrap(File {
                filename: "hello".to_owned(),
                size: 100,
            })))],
        });
        let batata_dir = wrap(Directory {
            name: "batata".to_owned(),
            entities: vec![
                wrap(DirectoryEntity::File(wrap(File {
                    filename: "hello 2".to_owned(),
                    size: 200,
                }))),
                wrap(DirectoryEntity::File(wrap(File {
                    filename: "hello 3".to_owned(),
                    size: 300,
                }))),
            ],
            parent: Some(root.clone()),
        });

        {
            root.borrow_mut()
                .entities
                .push(wrap(DirectoryEntity::Dir(batata_dir.clone())));
        }

        Filesystem {
            current_dir: batata_dir,
            root,
        }
    }

    #[test]
    fn build_fs() {
        let commands: Vec<CommandWithOutput> = vec![
            CommandWithOutput::CD(super::CDDestination::Root),
            CommandWithOutput::LS(vec![
                OutputDirectoryEntity::File((100, "hello".to_owned())),
                OutputDirectoryEntity::Dir("batata".to_owned()),
            ]),
            CommandWithOutput::CD(super::CDDestination::Child("batata".to_owned())),
            CommandWithOutput::LS(vec![
                OutputDirectoryEntity::File((200, "hello 2".to_owned())),
                OutputDirectoryEntity::File((300, "hello 3".to_owned())),
            ]),
        ];
        let fs = Filesystem::build(commands.into_iter());

        let expected_fs = example_fs();

        assert_eq!(fs, expected_fs)
    }

    #[test]
    fn fs_size() {
        let fs = example_fs();

        assert_eq!(fs.root.borrow().total_size(), 600);
    }
}
