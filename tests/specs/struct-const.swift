struct Test {
    let testProp : String
	let testProp2 : [String:String]!
    let testTuple : (s : String, [Int], UInt)
    let arrowTest : ((String -> Bool -> String) -> Void)

    
    func testFunc<A>(x : String) -> Void {
    }
    
}
