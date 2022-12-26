%token <int> INT
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token COMMA
%token EOF
%start <Packet.t> packet
%%

packet:
  | v = INT { Packet.Int v }
  | LEFT_BRACKET; packet = separated_list(COMMA, packet); RIGHT_BRACKET; { Packet.List packet }
