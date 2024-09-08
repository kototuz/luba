use std::path::{PathBuf, Path};
use std::fs;
use std::process::exit;
use std::io::prelude::*;

const DATAPACK_DESC: &str  = "nothing";
const DATAPACK_VERSION: u8 = 48;

pub struct Datapack {
    funcs_path: PathBuf
}

impl Datapack {
    pub fn create(name: &str) -> Self {

        let mut path_buf = PathBuf::from(name);

        path_buf.push("data");
        path_buf.push(name);
        path_buf.push("function");

        let ret = Self { funcs_path: path_buf.clone() };

        let _ = fs::create_dir_all(path_buf.clone()).inspect_err(|err| {
            eprintln!("ERROR: could not create datapack tree `{name}`: {err}");
            exit(1);
        });

        path_buf.clear();
        path_buf.push(name);
        path_buf.push("pack.mcmeta");
        let mut mcmeta = fs::File::create(path_buf.clone()).unwrap_or_else(|err| {
            eprintln!("ERROR: could not create `pack.mcmeta`: {err}");
            exit(1);
        });

        let _ = mcmeta.write_fmt(format_args!("\
            \"pack\": {{\
                \"description\": \"{}\",\
                \"pack_format\": {}\
            }}\n\
        ", DATAPACK_DESC, DATAPACK_VERSION)).inspect_err(|err| {
            eprintln!("ERROR: could not write `pack.mcmeta`: {err}");
            exit(1);
        });

        ret
    }

    pub fn create_fn_file(&self, name: &str) -> fs::File {
        let mut path = self.funcs_path.clone();
        path.push(name);
        fs::File::create(path.with_extension("mcfunction")).unwrap_or_else(|err| {
            eprintln!("ERROR: could not create `{name}.mcfunction`: {err}");
            exit(1);
        })
    }
}

// TODO: now we don't need to create a hole datapack
