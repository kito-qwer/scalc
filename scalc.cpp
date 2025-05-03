#include <iostream>
#include <fstream>
#include <vector>
#include <cctype>
#include <string>
#include <unordered_map>
#include <cmath>
#include <cstdlib>
#include <stdexcept>

#define APP_VERSION "0.1.1"
#define MAX_DEPTH 128

enum class TokenType {
	NUMBER,
	IDENTIFIER,
	PLUS,
	MINUS,
	MULTIPLY,
	DIVIDE,
	POW,
	LPAREN,
	RPAREN,
	EQUAL,
	COMMA,
	END
};

struct Token {
	TokenType type;
	std::string value;
	Token(TokenType t, std::string v = "") : type(t), value(v) {}
};

class Lexer {
	std::string input;
	size_t pos = 0;
public:
	Lexer(const std::string &expr) : input(expr) {}
	Token getNextToken() {
		while(pos < input.size() && isspace(input[pos])){ pos++; }
		if(pos >= input.size()) return Token(TokenType::END);
		char ch = input[pos];
		if(isdigit(ch) || ch == '.') {
			std::string num;
			while(pos < input.size() && (isdigit(input[pos]) || input[pos] == '.')) {
				num += input[pos++];
			}
			return Token(TokenType::NUMBER, num);
		}
		if(isalpha(ch)) {
			std::string ident;
			while(pos < input.size() && (isalnum(input[pos]) || input[pos] == '_')) {
				ident += input[pos++];
			}
			return Token(TokenType::IDENTIFIER, ident);
		}
		pos++;
		switch(ch) {
			case '+': 
				return Token(TokenType::PLUS);
			case '-': 
				return Token(TokenType::MINUS);
			case '*': 
				return Token(TokenType::MULTIPLY);
			case '/':
				return Token(TokenType::DIVIDE);
			case '(':
				return Token(TokenType::LPAREN);
			case ')':
				return Token(TokenType::RPAREN);
			case '=':
				return Token(TokenType::EQUAL);
			case ',':
				return Token(TokenType::COMMA);
		}
		throw std::runtime_error("Unexpected character: " + std::string(1, ch));
	}
};

using umapsd = std::unordered_map<std::string, double>;

struct ASTNode {
	virtual ~ASTNode() = default;
	virtual double evaluate(umapsd &variables) = 0;
};

struct NumberNode : public ASTNode {
	double value;
	NumberNode(double v) : value(v) {};
	double evaluate(umapsd &variables) override {
		return value;
	} 
};

struct VariableNode : public ASTNode {
	std::string name;
	VariableNode(const std::string &n) : name(n) {}
	double evaluate(umapsd &variables) override {
		if(variables.find(name) == variables.end()) throw std::runtime_error("Undefined variable: " + name);
		return variables[name];
	}
};

struct AssignmentNode : public ASTNode {
	std::string name;
	ASTNode *value;
	AssignmentNode(const std::string &n, ASTNode *v) : name(n), value(v) {}
	double evaluate(umapsd &variables) override {
		double val = value->evaluate(variables);
		variables[name] = val;
		return val;
	}
};

struct FunctionCallNode : public ASTNode {
	std::string functionName;
	std::vector<ASTNode *> arguments;
	FunctionCallNode(const std::string &name, std::vector<ASTNode *> args)
		: functionName(name), arguments(std::move(args)) {}
	double evaluate(umapsd &variables) override {
		if(arguments.size() == 1){
			if(functionName == "sin") return std::sin(arguments[0]->evaluate(variables));
			if(functionName == "cos") return std::cos(arguments[0]->evaluate(variables));
			if(functionName == "tan") return std::tan(arguments[0]->evaluate(variables));
			if(functionName == "asin") return std::asin(arguments[0]->evaluate(variables));
			if(functionName == "acos") return std::acos(arguments[0]->evaluate(variables));
			if(functionName == "atan") return std::atan(arguments[0]->evaluate(variables));

			if(functionName == "sinh") return std::sinh(arguments[0]->evaluate(variables));
			if(functionName == "cosh") return std::cosh(arguments[0]->evaluate(variables));
			if(functionName == "tanh") return std::tanh(arguments[0]->evaluate(variables));
			if(functionName == "asinh") return std::asinh(arguments[0]->evaluate(variables));
			if(functionName == "acosh") return std::acosh(arguments[0]->evaluate(variables));
			if(functionName == "atanh") return std::atanh(arguments[0]->evaluate(variables));

			if(functionName == "sqrt") return std::sqrt(arguments[0]->evaluate(variables));
			if(functionName == "cbrt") return std::cbrt(arguments[0]->evaluate(variables));
			if(functionName == "exp") return std::exp(arguments[0]->evaluate(variables));
			if(functionName == "ln") return std::log(arguments[0]->evaluate(variables));
			if(functionName == "log10") return std::log10(arguments[0]->evaluate(variables));
			if(functionName == "log2") return std::log2(arguments[0]->evaluate(variables));

			if(functionName == "abs") return std::abs(arguments[0]->evaluate(variables));
		}
		else if(arguments.size() == 2) {
			if(functionName == "log") return std::log(arguments[1]->evaluate(variables)) / std::log(arguments[0]->evaluate(variables));
			if(functionName == "pow") return std::pow(arguments[0]->evaluate(variables), arguments[1]->evaluate(variables));
			if(functionName == "mod") return std::fmod(arguments[0]->evaluate(variables), arguments[1]->evaluate(variables));
		}
		throw std::runtime_error("Unknown function: " + functionName);
	}
};

struct UnaryOpNode : public ASTNode {
	TokenType op;
	ASTNode *operand;
	UnaryOpNode(TokenType o, ASTNode *expr) : op(o), operand(expr) {}
	double evaluate(umapsd &variables) override {
		double val = operand->evaluate(variables);
		switch(op) {
			case TokenType::MINUS:
				return -val;
			default :
				throw std::runtime_error("Invalid unary operator");
		}
	}
};

struct BinaryOpNode : public ASTNode {
	TokenType op;
	ASTNode *left, *right;
	BinaryOpNode(TokenType o, ASTNode *l, ASTNode *r) : op(o), left(l), right(r) {}
public:
	double evaluate(umapsd &variables) override {
		double lval = left->evaluate(variables);
		double rval = right->evaluate(variables);
		switch(op) {
			case TokenType::PLUS:
				return lval + rval;
			case TokenType::MINUS:
				return lval - rval;
			case TokenType::MULTIPLY:
				return lval * rval;
			case TokenType::DIVIDE:
				return lval / rval;
			default:
				throw std::runtime_error("Invalid operator");
		}
	}
};

class Parser {
	Lexer lexer;
	Token curtToken;
	void consume(TokenType type) {
		if(curtToken.type == type)curtToken = lexer.getNextToken();
		else throw std::runtime_error("Unexpected token: " + curtToken.value);
	}
public:
	Parser(const std::string &expr) : lexer(expr), curtToken(lexer.getNextToken()) {}
	ASTNode *parseExpression() {
		ASTNode *node = parseTerm();
		while(curtToken.type ==  TokenType::PLUS || curtToken.type == TokenType::MINUS) {
			TokenType op = curtToken.type;
			consume(op);
			node = new BinaryOpNode(op, node, parseTerm());
		}
		return node;
	}
	ASTNode *parseTerm() {
		ASTNode *node = parseFactor();
		while(curtToken.type == TokenType::MULTIPLY || curtToken.type == TokenType::DIVIDE) {
			TokenType op = curtToken.type;
			consume(op);
			node = new BinaryOpNode(op, node, parseFactor());
		}
		return node;
	}
	ASTNode *parseFactor() {
		if(curtToken.type == TokenType::MINUS) {
			consume(TokenType::MINUS);
			return new UnaryOpNode(TokenType::MINUS, parseFactor());
		}
		if(curtToken.type == TokenType::NUMBER) {
			double value = std::stod(curtToken.value);
			consume(TokenType::NUMBER);
			return new NumberNode(value);
		}
		else if(curtToken.type == TokenType::IDENTIFIER) {
			std::string name = curtToken.value;
			consume(TokenType::IDENTIFIER);
			if(curtToken.type == TokenType::LPAREN) {
				return parseFunctionCall(name);
			}
			else if(curtToken.type == TokenType::EQUAL) {
				consume(TokenType::EQUAL);
				return new AssignmentNode(name, parseExpression());
			}
			return new VariableNode(name);
		}
		else if(curtToken.type == TokenType::LPAREN) {
			consume(TokenType::LPAREN);
			ASTNode *node = parseExpression();
			consume(TokenType::RPAREN);
			return node;
		}
		throw std::runtime_error("Unexpected token: " + curtToken.value);
	}
	ASTNode *parseFunctionCall(const std::string &funcName) {
		consume(TokenType::LPAREN);
		std::vector<ASTNode *> args;
		if(curtToken.type != TokenType::RPAREN) {
			do {
				args.push_back(parseExpression());
				if(curtToken.type == TokenType::COMMA) { consume(TokenType::COMMA); }
				else { break; }
			} while(true);
		}
		consume(TokenType::RPAREN);
		return new FunctionCallNode(funcName, args);
	}
};

struct Options {
	std::vector<std::string> args;
	bool help = false, version = false, once = false, file = false;
	bool exit = false;
	std::vector<std::string> files = { "init.scalc" };
	Options(int argc, char **argv) {
		for(int i = 1; i < argc; i++) {
			args.push_back(argv[i]);
			if(args.back() == "-h" || args.back() == "--help") {
				exit = help = true;
				output::help();
				break;
			}
			if(args.back() == "-v" || args.back() == "--version") {
				exit = version = true;
				output::version();
				break;
			}
			if(args.back() == "-o" || args.back() == "--once") {
				once = true;
			}
			if(args.back() == "-f" || args.back() == "--file") {
				if(++i < argc){
					args.push_back(argv[i]);
					files.push_back(args.back());
				}
			}
		}
	}
	struct output {
		static void help() {
			std::string usage;
			usage  = "Usage: scalc [options]\n";
			usage += "Options:\n";
			usage += "  -h --help         Display this information.\n";
			usage += "  -v --version      Display calculator version information.\n";
			usage += "  -o --once         Run the calculation only once and then exit.\n";
			usage += "  -f <path>\n";
			usage += "    --file <path>   Execute commands from specified file.\n";
			usage += "Interactive commands:\n";
			usage += "  :e :exit          Exit interactive mode.\n";
			usage += "  :h :help          Display this information.\n";
			usage += "  :f <paths>\n";
			usage += "    :file <paths>   Execute commands from specified files.\n";
			usage += "  <expression>      Calculate expression. The result is stored variable 'Ans'.\n";
			std::cout << usage << std::flush;
		}
		static void version() {
			std::string ver;
			ver  = "scalc " APP_VERSION "\n";
			ver += "Copyright (C) 2025 Qvito\n";
			std::cout << ver << std::flush;
		}
	};
};

double calculate(std::string line, umapsd& variables){
	Parser parser(line);
	ASTNode *expr = parser.parseExpression();
	double value = expr->evaluate(variables);
	delete expr;
	return value;
}

std::vector<std::string> commandsDivide(const std::string& input) {
	std::vector<std::string> terms;
	if (input.empty() || input[0] != ':') return terms;
	std::string remaining = input.substr(1);
	std::string term;
	bool in_quotes = false;
	char quote_char = '\0';
	bool escape = false;
	for (size_t i = 0; i < remaining.length(); ++i) {
		char c = remaining[i];
		if (escape) {
			term += c;
			escape = false;
			continue;
		}
		if (c == '\\') {
			escape = true;
			continue;
		}
		if ((c == '"' || c == '\'') && !escape) {
			if (in_quotes) {
				if (c == quote_char) {
					in_quotes = false;
					quote_char = '\0';
					continue;
				}
				else {
					term += c;
				}
			}
			else {
				in_quotes = true;
				quote_char = c;
			}
			continue;
		}
		if (c == ' ' && !in_quotes) {
			if (!term.empty()) {
				terms.push_back(term);
				term.clear();
			}
		}
		else {
			term += c;
		}
	}
	if (in_quotes) {
		throw std::runtime_error("Unclosed quote in input string.");
	}
	if (!term.empty()) {
		terms.push_back(term);
	}
	return terms;
}

void process(std::istream& stream, bool write, umapsd& variables, Options& opts, int depth) {
	if(MAX_DEPTH < depth)return;
	std::string line;
	do {
		if(write)std::cout << "> ";
		if(not std::getline(stream, line))break;
		if(line.empty())continue;
		if(line[0] == ':'){
			auto terms = commandsDivide(line);
			if(terms.empty())continue;
			if(terms[0] == "e" || terms[0] == "exit") {
				break;
			}
			if(terms[0] == "h" || terms[0] == "help") {
				if(write)Options::output::help();
			}
			if(terms[0] == "f" || terms[0] == "file") {
				for(int i = 1; i < int(terms.size()); i++) {
					std::ifstream file(terms[i]);
					if (file.is_open()) {
						process(file, false, variables, opts, depth + 1);
					}
					else std::cerr << "\033[31mError: Cannot open file " << terms[i] << "\033[0m" << std::endl;
				}
			}
			continue;
		}
		try {
			double result = calculate("Ans = " + line, variables);
			if(write)std::cout << "Ans: " << result << std::endl;
		}
		catch(const std::exception &e) {
			std::cerr << "\033[31m" << "Error: " << e.what() << "\033[0m" << std::endl;
		}
	} while(not opts.once || not write);
}

int main(int argc, char **argv){
	Options opts(argc, argv);
	if(opts.exit) { return 0; }
	umapsd variables;
	variables["Ans"] = 0.0;
	for(auto optfile : opts.files){
		std::ifstream initfile(optfile);
		if(initfile.is_open()) {
			process(initfile, false, variables, opts, 0);
		}
	}
	process(std::cin, true, variables, opts, 0);
	return 0;
}
