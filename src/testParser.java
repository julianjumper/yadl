// Generated from test.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class testParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		Statment=1, Declaration=2, Format=3, DataEntries=4, Save=5, Value=6, ValueOps=7, 
		ValueExpr=8, DataAccess=9, Load=10, Filetype=11, Filename=12, Querry=13, 
		QuerryOps=14, Filter=15, Condition=16, ConditionOps=17, Map=18, Mapping=19, 
		Self=20, Reduce=21, ReduceExpr=22, Join=23, Slice=24, SliceSize=25, TimeUnit=26, 
		Identifier=27, IdentifierStart=28, Integer=29, Newline=30, Type=31, WS=32;
	public static final int
		RULE_test = 0;
	private static String[] makeRuleNames() {
		return new String[] {
			"test"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, "'d'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "Statment", "Declaration", "Format", "DataEntries", "Save", "Value", 
			"ValueOps", "ValueExpr", "DataAccess", "Load", "Filetype", "Filename", 
			"Querry", "QuerryOps", "Filter", "Condition", "ConditionOps", "Map", 
			"Mapping", "Self", "Reduce", "ReduceExpr", "Join", "Slice", "SliceSize", 
			"TimeUnit", "Identifier", "IdentifierStart", "Integer", "Newline", "Type", 
			"WS"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "test.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public testParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TestContext extends ParserRuleContext {
		public TerminalNode EOF() { return getToken(testParser.EOF, 0); }
		public List<TerminalNode> WS() { return getTokens(testParser.WS); }
		public TerminalNode WS(int i) {
			return getToken(testParser.WS, i);
		}
		public List<TerminalNode> Newline() { return getTokens(testParser.Newline); }
		public TerminalNode Newline(int i) {
			return getToken(testParser.Newline, i);
		}
		public List<TerminalNode> Statment() { return getTokens(testParser.Statment); }
		public TerminalNode Statment(int i) {
			return getToken(testParser.Statment, i);
		}
		public TestContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_test; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof testListener ) ((testListener)listener).enterTest(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof testListener ) ((testListener)listener).exitTest(this);
		}
	}

	public final TestContext test() throws RecognitionException {
		TestContext _localctx = new TestContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_test);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(9);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Statment || _la==WS) {
				{
				{
				setState(3);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Statment) {
					{
					setState(2);
					match(Statment);
					}
				}

				setState(5);
				match(WS);
				setState(6);
				match(Newline);
				}
				}
				setState(11);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(12);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\u0004\u0001 \u000f\u0002\u0000\u0007\u0000\u0001\u0000\u0003\u0000\u0004"+
		"\b\u0000\u0001\u0000\u0001\u0000\u0005\u0000\b\b\u0000\n\u0000\f\u0000"+
		"\u000b\t\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0000\u0000\u0001\u0000"+
		"\u0000\u0000\u000f\u0000\t\u0001\u0000\u0000\u0000\u0002\u0004\u0005\u0001"+
		"\u0000\u0000\u0003\u0002\u0001\u0000\u0000\u0000\u0003\u0004\u0001\u0000"+
		"\u0000\u0000\u0004\u0005\u0001\u0000\u0000\u0000\u0005\u0006\u0005 \u0000"+
		"\u0000\u0006\b\u0005\u001e\u0000\u0000\u0007\u0003\u0001\u0000\u0000\u0000"+
		"\b\u000b\u0001\u0000\u0000\u0000\t\u0007\u0001\u0000\u0000\u0000\t\n\u0001"+
		"\u0000\u0000\u0000\n\f\u0001\u0000\u0000\u0000\u000b\t\u0001\u0000\u0000"+
		"\u0000\f\r\u0005\u0000\u0000\u0001\r\u0001\u0001\u0000\u0000\u0000\u0002"+
		"\u0003\t";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}