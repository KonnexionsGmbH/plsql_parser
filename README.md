# plsql_parser - LALR grammar based PL/SQL parser

![Travis (.org)](https://img.shields.io/travis/K2InformaticsGmbH/plsql_parser.svg?style=popout-square)
![Coveralls github](https://img.shields.io/coveralls/github/K2InformaticsGmbH/plsql_parser.svg?style=popout-square)
![GitHub](https://img.shields.io/github/license/K2InformaticsGmbH/plsql_parser.svg?style=popout-square)
![GitHub release](https://img.shields.io/github/release/K2InformaticsGmbH/plsql_parser.svg?style=popout-square)
![GitHub Release Date](https://img.shields.io/github/release-date/K2InformaticsGmbH/plsql_parser.svg?style=popout-square)
![GitHub commits since latest release](https://img.shields.io/github/commits-since/K2InformaticsGmbH/plsql_parser/1.2.0.svg?style=popout-square)

**plsql_parser** is a production-ready SQL parser written in pure Erlang. **plsql_parser** is aligned to the Oracle PL/SQL language and enriched with some specific features.

## 1. Usage

### Example code:

```
CREATE PACKAGE my_package
IS
    FUNCTION my_function 
        RETURN BOOLEAN;
END my_package;
```

### Parsing the example code:

```erlang
1> {ok, {ParseTree, Tokens}} = plsql_parser:parsetree_with_tokens("CREATE PACKAGE my_package IS FUNCTION my_function RETURN BOOLEAN; END my_package;").
{ok,{[#{plsqlUnit =>
            #{createPackage@ =>
                  #{createPackage =>
                        #{plsqlPackageSource@ =>
                              #{plsqlPackageSource =>
                                    #{asIs@ => "IS",
                                      packageItemList@ =>
                                          [#{packageFunctionDeclaration =>
                                                 #{functionHeading@ =>
                                                       #{functionHeading =>
                                                             #{name@ =>
                                                                   "my_function",
                                                               return@ =>
                                                                   #{dataType =>
                                                                         #{class@ =>
                                                                               plsql,
                                                                           type@ =>
                                                                               "BOOLEAN"}}}}}}],
                                      packageNameEnd@ => "my_package",
                                      packageNameStart@ => "my_package"}}}}}}],
     [{'CREATE',1},
      {'PACKAGE',1},
      {'NAME',10,"my_package"},
      {'IS',1},
      {'FUNCTION',1},
      {'NAME',11,"my_function"},
      {'RETURN',1},
      {'BOOLEAN',1},
      {';',1},
      {'END',1},
      {'NAME',10,"my_package"},
      {';',1}]}}
```

### Access the parse tree of the example code:

```erlang
2> ParseTree.
[#{plsqlUnit =>
       #{createPackage@ =>
             #{createPackage =>
                   #{plsqlPackageSource@ =>
                         #{plsqlPackageSource =>
                               #{asIs@ => "IS",
                                 packageItemList@ =>
                                     [#{packageFunctionDeclaration =>
                                            #{functionHeading@ =>
                                                  #{functionHeading =>
                                                        #{name@ =>
                                                              "my_function",
                                                          return@ =>
                                                              #{dataType =>
                                                                    #{class@ =>
                                                                          plsql,
                                                                      type@ =>
                                                                          "BOOLEAN"}}}}}}],
                                 packageNameEnd@ => "my_package",
                                 packageNameStart@ => "my_package"}}}}}}]
```

### Access the token list of the example code:

```erlang
3> Tokens.
[{'CREATE',1},
 {'PACKAGE',1},
 {'NAME',10,"my_package"},
 {'IS',1},
 {'FUNCTION',1},
 {'NAME',11,"my_function"},
 {'RETURN',1},
 {'BOOLEAN',1},
 {';',1},
 {'END',1},
 {'NAME',10,"my_package"},
 {';',1}]
```

### Compile the code from a parse tree:

```erlang
4> plsql_parser_fold:top_down(plsql_parser_format_flat, ParseTree, []).
<<"create package my_package IS function my_function return BOOLEAN; end my_package;">>
```

## 2. Documentation

The documentation for **plsql_parser** is available here: [Wiki](https://github.com/K2InformaticsGmbH/plsql_parser/wiki).

## 3. Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
