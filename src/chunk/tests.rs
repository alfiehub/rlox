use super::*;

#[test]
fn test_chunk_write() {
    let mut chunk = Chunk::default();

    // Test single write
    chunk.write(1, 123);
    assert_eq!(chunk.count, 1);
    assert_eq!(chunk.code[0], 1);

    // Test multiple writes
    chunk.write(2, 123);
    chunk.write(3, 123);
    assert_eq!(chunk.count, 3);
    assert_eq!(chunk.code, vec![1, 2, 3]);

    // Test capacity growth
    // Initial capacity should be 0
    assert!(chunk.capacity >= chunk.count);
    assert_eq!(chunk.capacity, 8);

    // Write enough bytes to force multiple capacity increases
    for i in 0..10 {
        chunk.write(i, 123);
    }

    // Verify all bytes were written correctly
    assert_eq!(chunk.count, 13); // 3 from before + 10 new ones
    assert!(chunk.capacity >= chunk.count);
    assert_eq!(chunk.capacity, 16);
}
