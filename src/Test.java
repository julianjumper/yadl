import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.pattern.ParseTreeMatch;

import java.io.IOException;
import java.nio.file.Path;

class Test {
    public static void main(String[] args) {
        if ( args.length < 1 ) {
            System.out.println("too few arguments");
            return;
        }

        var path =  Path.of(args[0]);
        try {
            var lexer = new testLexer(CharStreams.fromPath(path));
            var parser = new testParser(new BufferedTokenStream(lexer) );
            var nodes = parser.test().Statment();
            for (var statement : nodes) {
                var count = statement.getChildCount();
                for (int i = 0; i < count; ++i) {
                    var child = statement.getChild(i);
                    System.out.println(child + "\n --------------------------");
                }
            }

        } catch (IOException | RecognitionException e) {
            System.out.println(e);
        }
    }
}