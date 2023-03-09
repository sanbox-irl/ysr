mod proto;

#[cfg(test)]
mod tests {
    use super::*;
    use prost::{bytes::Buf, Message};

    const TEST_PROGRAM: &[u8] = include_bytes!("../proto/test.yarnc");

    #[test]
    fn test() {
        let program = proto::Program::decode(TEST_PROGRAM).unwrap();
        println!("{:#?}", program);

        panic!()
    }
}
