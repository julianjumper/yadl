// Generated from test.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link testParser}.
 */
public interface testListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link testParser#test}.
	 * @param ctx the parse tree
	 */
	void enterTest(testParser.TestContext ctx);
	/**
	 * Exit a parse tree produced by {@link testParser#test}.
	 * @param ctx the parse tree
	 */
	void exitTest(testParser.TestContext ctx);
}