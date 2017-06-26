describe "hylang grammar", ->
  grammar = null

  beforeEach ->
    waitsForPromise ->
      atom.packages.activatePackage("language-hylang")

    runs ->
      grammar = atom.grammars.grammarForScopeName("source.hylang")

  it "parses the grammar", ->
    expect(grammar).toBeDefined()
    expect(grammar.scopeName).toBe "source.hylang"

  it "tokenizes semicolon comments", ->
    {tokens} = grammar.tokenizeLine "; hylang"
    expect(tokens[0]).toEqual value: ";", scopes: ["source.hylang", "comment.line.semicolon.hylang", "punctuation.definition.comment.hylang"]
    expect(tokens[1]).toEqual value: " hylang", scopes: ["source.hylang", "comment.line.semicolon.hylang"]

  it "does not tokenize escaped semicolons as comments", ->
    {tokens} = grammar.tokenizeLine "\\; hylang"
    expect(tokens[0]).toEqual value: "\\; ", scopes: ["source.hylang"]
    expect(tokens[1]).toEqual value: "hylang", scopes: ["source.hylang", "meta.symbol.hylang"]

  it "tokenizes shebang comments", ->
    {tokens} = grammar.tokenizeLine "#!/usr/bin/env hy"
    expect(tokens[0]).toEqual value: "#!", scopes: ["source.hylang", "comment.line.shebang.hylang", "punctuation.definition.comment.shebang.hylang"]
    expect(tokens[1]).toEqual value: "/usr/bin/env hy", scopes: ["source.hylang", "comment.line.shebang.hylang"]

  it "tokenizes strings", ->
    {tokens} = grammar.tokenizeLine '"foo bar"'
    expect(tokens[0]).toEqual value: '"', scopes: ["source.hylang", "string.quoted.double.hylang", "punctuation.definition.string.begin.hylang"]
    expect(tokens[1]).toEqual value: 'foo bar', scopes: ["source.hylang", "string.quoted.double.hylang"]
    expect(tokens[2]).toEqual value: '"', scopes: ["source.hylang", "string.quoted.double.hylang", "punctuation.definition.string.end.hylang"]

  it "tokenizes character escape sequences", ->
    {tokens} = grammar.tokenizeLine '"\\n"'
    expect(tokens[0]).toEqual value: '"', scopes: ["source.hylang", "string.quoted.double.hylang", "punctuation.definition.string.begin.hylang"]
    expect(tokens[1]).toEqual value: '\\n', scopes: ["source.hylang", "string.quoted.double.hylang", "constant.character.escape.hylang"]
    expect(tokens[2]).toEqual value: '"', scopes: ["source.hylang", "string.quoted.double.hylang", "punctuation.definition.string.end.hylang"]

  it "tokenizes regexes", ->
    {tokens} = grammar.tokenizeLine '#"foo"'
    expect(tokens[0]).toEqual value: '#"', scopes: ["source.hylang", "string.regexp.hylang", "punctuation.definition.regexp.begin.hylang"]
    expect(tokens[1]).toEqual value: 'foo', scopes: ["source.hylang", "string.regexp.hylang"]
    expect(tokens[2]).toEqual value: '"', scopes: ["source.hylang", "string.regexp.hylang", "punctuation.definition.regexp.end.hylang"]

  it "tokenizes backslash escape character in regexes", ->
    {tokens} = grammar.tokenizeLine '#"\\\\" "/"'
    expect(tokens[0]).toEqual value: '#"', scopes: ["source.hylang", "string.regexp.hylang", "punctuation.definition.regexp.begin.hylang"]
    expect(tokens[1]).toEqual value: "\\\\", scopes: ['source.hylang', 'string.regexp.hylang', 'constant.character.escape.hylang']
    expect(tokens[2]).toEqual value: '"', scopes: ['source.hylang', 'string.regexp.hylang', "punctuation.definition.regexp.end.hylang"]
    expect(tokens[4]).toEqual value: '"', scopes: ['source.hylang', 'string.quoted.double.hylang', 'punctuation.definition.string.begin.hylang']
    expect(tokens[5]).toEqual value: "/", scopes: ['source.hylang', 'string.quoted.double.hylang']
    expect(tokens[6]).toEqual value: '"', scopes: ['source.hylang', 'string.quoted.double.hylang', 'punctuation.definition.string.end.hylang']

  it "tokenizes escaped double quote in regexes", ->
    {tokens} = grammar.tokenizeLine '#"\\""'
    expect(tokens[0]).toEqual value: '#"', scopes: ["source.hylang", "string.regexp.hylang", "punctuation.definition.regexp.begin.hylang"]
    expect(tokens[1]).toEqual value: '\\"', scopes: ['source.hylang', 'string.regexp.hylang', 'constant.character.escape.hylang']
    expect(tokens[2]).toEqual value: '"', scopes: ['source.hylang', 'string.regexp.hylang', "punctuation.definition.regexp.end.hylang"]

  it "tokenizes numerics", ->
    numbers =
      "constant.numeric.ratio.hylang": ["1/2", "123/456"]
      "constant.numeric.hexadecimal.hylang": ["0xDEADBEEF", "0XDEADBEEF"]
      "constant.numeric.octal.hylang": ["0123"]
      "constant.numeric.bigdecimal.hylang": ["123.456M"]
      "constant.numeric.double.hylang": ["123.45", "123.45e6", "123.45E6"]
      "constant.numeric.bigint.hylang": ["123N"]
      "constant.numeric.long.hylang": ["123", "12321"]

    for scope, nums of numbers
      for num in nums
        {tokens} = grammar.tokenizeLine num
        expect(tokens[0]).toEqual value: num, scopes: ["source.hylang", scope]

  it "tokenizes booleans", ->
    booleans =
      "constant.language.boolean.hylang": ["true", "false"]

    for scope, bools of booleans
      for bool in bools
        {tokens} = grammar.tokenizeLine bool
        expect(tokens[0]).toEqual value: bool, scopes: ["source.hylang", scope]

  it "tokenizes nil", ->
    {tokens} = grammar.tokenizeLine "nil"
    expect(tokens[0]).toEqual value: "nil", scopes: ["source.hylang", "constant.language.nil.hylang"]

  it "tokenizes keywords", ->
    tests =
      "meta.expression.hylang": ["(:foo)"]
      "meta.map.hylang": ["{:foo}"]
      "meta.vector.hylang": ["[:foo]"]
      "meta.quoted-expression.hylang": ["'(:foo)", "`(:foo)"]

    for metaScope, lines of tests
      for line in lines
        {tokens} = grammar.tokenizeLine line
        expect(tokens[1]).toEqual value: ":foo", scopes: ["source.hylang", metaScope, "constant.keyword.hylang"]

  it "tokenizes keyfns (keyword control)", ->
    keyfns = ["declare", "declare-", "ns", "in-ns", "import", "use", "require", "load", "compile", "def", "defn", "defn-", "defmacro"]

    for keyfn in keyfns
      {tokens} = grammar.tokenizeLine "(#{keyfn})"
      expect(tokens[1]).toEqual value: keyfn, scopes: ["source.hylang", "meta.expression.hylang", "keyword.control.hylang"]

  it "tokenizes keyfns (storage control)", ->
    keyfns = ["if", "when", "for", "cond", "do", "let", "binding", "loop", "recur", "fn", "throw", "try", "catch", "finally", "case"]

    for keyfn in keyfns
      {tokens} = grammar.tokenizeLine "(#{keyfn})"
      expect(tokens[1]).toEqual value: keyfn, scopes: ["source.hylang", "meta.expression.hylang", "storage.control.hylang"]

  it "tokenizes global definitions", ->
    macros = ["ns", "declare", "def", "defn", "defn-", "defroutes", "compojure/defroutes", "rum.core/defc123-", "some.nested-ns/def-nested->symbol!?*", "def+!.?abc8:<>", "ns/def+!.?abc8:<>"]

    for macro in macros
      {tokens} = grammar.tokenizeLine "(#{macro} foo 'bar)"
      expect(tokens[1]).toEqual value: macro, scopes: ["source.hylang", "meta.expression.hylang", "meta.definition.global.hylang", "keyword.control.hylang"]
      expect(tokens[3]).toEqual value: "foo", scopes: ["source.hylang", "meta.expression.hylang", "meta.definition.global.hylang", "entity.global.hylang"]

  it "tokenizes dynamic variables", ->
    mutables = ["*ns*", "*foo-bar*"]

    for mutable in mutables
      {tokens} = grammar.tokenizeLine mutable
      expect(tokens[0]).toEqual value: mutable, scopes: ["source.hylang", "meta.symbol.dynamic.hylang"]

  it "tokenizes metadata", ->
    {tokens} = grammar.tokenizeLine "^Foo"
    expect(tokens[0]).toEqual value: "^", scopes: ["source.hylang", "meta.metadata.simple.hylang"]
    expect(tokens[1]).toEqual value: "Foo", scopes: ["source.hylang", "meta.metadata.simple.hylang", "meta.symbol.hylang"]

    {tokens} = grammar.tokenizeLine "^{:foo true}"
    expect(tokens[0]).toEqual value: "^{", scopes: ["source.hylang", "meta.metadata.map.hylang", "punctuation.section.metadata.map.begin.hylang"]
    expect(tokens[1]).toEqual value: ":foo", scopes: ["source.hylang", "meta.metadata.map.hylang", "constant.keyword.hylang"]
    expect(tokens[2]).toEqual value: " ", scopes: ["source.hylang", "meta.metadata.map.hylang"]
    expect(tokens[3]).toEqual value: "true", scopes: ["source.hylang", "meta.metadata.map.hylang", "constant.language.boolean.hylang"]
    expect(tokens[4]).toEqual value: "}", scopes: ["source.hylang", "meta.metadata.map.hylang", "punctuation.section.metadata.map.end.trailing.hylang"]

  it "tokenizes functions", ->
    expressions = ["(foo)", "(foo 1 10)"]

    for expr in expressions
      {tokens} = grammar.tokenizeLine expr
      expect(tokens[1]).toEqual value: "foo", scopes: ["source.hylang", "meta.expression.hylang", "entity.name.function.hylang"]

  it "tokenizes vars", ->
    {tokens} = grammar.tokenizeLine "(func #'foo)"
    expect(tokens[2]).toEqual value: " #", scopes: ["source.hylang", "meta.expression.hylang"]
    expect(tokens[3]).toEqual value: "'foo", scopes: ["source.hylang", "meta.expression.hylang", "meta.var.hylang"]

  it "tokenizes symbols", ->
    {tokens} = grammar.tokenizeLine "foo/bar"
    expect(tokens[0]).toEqual value: "foo", scopes: ["source.hylang", "meta.symbol.namespace.hylang"]
    expect(tokens[1]).toEqual value: "/", scopes: ["source.hylang"]
    expect(tokens[2]).toEqual value: "bar", scopes: ["source.hylang", "meta.symbol.hylang"]

  testMetaSection = (metaScope, puncScope, startsWith, endsWith) ->
    # Entire expression on one line.
    {tokens} = grammar.tokenizeLine "#{startsWith}foo, bar#{endsWith}"

    [start, mid..., end] = tokens

    expect(start).toEqual value: startsWith, scopes: ["source.hylang", "meta.#{metaScope}.hylang", "punctuation.section.#{puncScope}.begin.hylang"]
    expect(end).toEqual value: endsWith, scopes: ["source.hylang", "meta.#{metaScope}.hylang", "punctuation.section.#{puncScope}.end.trailing.hylang"]

    for token in mid
      expect(token.scopes.slice(0, 2)).toEqual ["source.hylang", "meta.#{metaScope}.hylang"]

    # Expression broken over multiple lines.
    tokens = grammar.tokenizeLines("#{startsWith}foo\n bar#{endsWith}")

    [start, mid..., after] = tokens[0]

    expect(start).toEqual value: startsWith, scopes: ["source.hylang", "meta.#{metaScope}.hylang", "punctuation.section.#{puncScope}.begin.hylang"]

    for token in mid
      expect(token.scopes.slice(0, 2)).toEqual ["source.hylang", "meta.#{metaScope}.hylang"]

    [mid..., end] = tokens[1]

    expect(end).toEqual value: endsWith, scopes: ["source.hylang", "meta.#{metaScope}.hylang", "punctuation.section.#{puncScope}.end.trailing.hylang"]

    for token in mid
      expect(token.scopes.slice(0, 2)).toEqual ["source.hylang", "meta.#{metaScope}.hylang"]

  it "tokenizes expressions", ->
    testMetaSection "expression", "expression", "(", ")"

  it "tokenizes quoted expressions", ->
    testMetaSection "quoted-expression", "expression", "'(", ")"
    testMetaSection "quoted-expression", "expression", "`(", ")"

  it "tokenizes vectors", ->
    testMetaSection "vector", "vector", "[", "]"

  it "tokenizes maps", ->
    testMetaSection "map", "map", "{", "}"

  it "tokenizes sets", ->
    testMetaSection "set", "set", "\#{", "}"

  it "tokenizes functions in nested sexp", ->
    {tokens} = grammar.tokenizeLine "((foo bar) baz)"
    expect(tokens[0]).toEqual value: "(", scopes: ["source.hylang", "meta.expression.hylang", "punctuation.section.expression.begin.hylang"]
    expect(tokens[1]).toEqual value: "(", scopes: ["source.hylang", "meta.expression.hylang", "meta.expression.hylang", "punctuation.section.expression.begin.hylang"]
    expect(tokens[2]).toEqual value: "foo", scopes: ["source.hylang", "meta.expression.hylang", "meta.expression.hylang", "entity.name.function.hylang"]
    expect(tokens[3]).toEqual value: " ", scopes: ["source.hylang", "meta.expression.hylang", "meta.expression.hylang"]
    expect(tokens[4]).toEqual value: "bar", scopes: ["source.hylang", "meta.expression.hylang", "meta.expression.hylang", "meta.symbol.hylang"]
    expect(tokens[5]).toEqual value: ")", scopes: ["source.hylang", "meta.expression.hylang", "meta.expression.hylang", "punctuation.section.expression.end.hylang"]
    expect(tokens[6]).toEqual value: " ", scopes: ["source.hylang", "meta.expression.hylang"]
    expect(tokens[7]).toEqual value: "baz", scopes: ["source.hylang", "meta.expression.hylang", "meta.symbol.hylang"]
    expect(tokens[8]).toEqual value: ")", scopes: ["source.hylang", "meta.expression.hylang", "punctuation.section.expression.end.trailing.hylang"]

  it "tokenizes maps used as functions", ->
    {tokens} = grammar.tokenizeLine "({:foo bar} :foo)"
    expect(tokens[0]).toEqual value: "(", scopes: ["source.hylang", "meta.expression.hylang", "punctuation.section.expression.begin.hylang"]
    expect(tokens[1]).toEqual value: "{", scopes: ["source.hylang", "meta.expression.hylang", "meta.map.hylang", "punctuation.section.map.begin.hylang"]
    expect(tokens[2]).toEqual value: ":foo", scopes: ["source.hylang", "meta.expression.hylang", "meta.map.hylang", "constant.keyword.hylang"]
    expect(tokens[3]).toEqual value: " ", scopes: ["source.hylang", "meta.expression.hylang", "meta.map.hylang"]
    expect(tokens[4]).toEqual value: "bar", scopes: ["source.hylang", "meta.expression.hylang", "meta.map.hylang", "meta.symbol.hylang"]
    expect(tokens[5]).toEqual value: "}", scopes: ["source.hylang", "meta.expression.hylang", "meta.map.hylang", "punctuation.section.map.end.hylang"]
    expect(tokens[6]).toEqual value: " ", scopes: ["source.hylang", "meta.expression.hylang"]
    expect(tokens[7]).toEqual value: ":foo", scopes: ["source.hylang", "meta.expression.hylang", "constant.keyword.hylang"]
    expect(tokens[8]).toEqual value: ")", scopes: ["source.hylang", "meta.expression.hylang", "punctuation.section.expression.end.trailing.hylang"]

  it "tokenizes sets used in functions", ->
    {tokens} = grammar.tokenizeLine "(\#{:foo :bar})"
    expect(tokens[0]).toEqual value: "(", scopes: ["source.hylang", "meta.expression.hylang", "punctuation.section.expression.begin.hylang"]
    expect(tokens[1]).toEqual value: "\#{", scopes: ["source.hylang", "meta.expression.hylang", "meta.set.hylang", "punctuation.section.set.begin.hylang"]
    expect(tokens[2]).toEqual value: ":foo", scopes: ["source.hylang", "meta.expression.hylang", "meta.set.hylang", "constant.keyword.hylang"]
    expect(tokens[3]).toEqual value: " ", scopes: ["source.hylang", "meta.expression.hylang", "meta.set.hylang"]
    expect(tokens[4]).toEqual value: ":bar", scopes: ["source.hylang", "meta.expression.hylang", "meta.set.hylang", "constant.keyword.hylang"]
    expect(tokens[5]).toEqual value: "}", scopes: ["source.hylang", "meta.expression.hylang", "meta.set.hylang", "punctuation.section.set.end.trailing.hylang"]
    expect(tokens[6]).toEqual value: ")", scopes: ["source.hylang", "meta.expression.hylang", "punctuation.section.expression.end.trailing.hylang"]

  describe "firstLineMatch", ->
    it "recognises interpreter directives", ->
      valid = """
        #!/usr/sbin/boot foo
        #!/usr/bin/boot foo=bar/
        #!/usr/sbin/boot
        #!/usr/sbin/boot foo bar baz
        #!/usr/bin/boot perl
        #!/usr/bin/boot bin/perl
        #!/usr/bin/boot
        #!/bin/boot
        #!/usr/bin/boot --script=usr/bin
        #! /usr/bin/env A=003 B=149 C=150 D=xzd E=base64 F=tar G=gz H=head I=tail boot
        #!\t/usr/bin/env --foo=bar boot --quu=quux
        #! /usr/bin/boot
        #!/usr/bin/env boot
      """
      for line in valid.split /\n/
        expect(grammar.firstLineRegex.scanner.findNextMatchSync(line)).not.toBeNull()

      invalid = """
        \x20#!/usr/sbin/boot
        \t#!/usr/sbin/boot
        #!/usr/bin/env-boot/node-env/
        #!/usr/bin/das-boot
        #! /usr/binboot
        #!\t/usr/bin/env --boot=bar
      """
      for line in invalid.split /\n/
        expect(grammar.firstLineRegex.scanner.findNextMatchSync(line)).toBeNull()

    it "recognises Emacs modelines", ->
      valid = """
        #-*- hylang -*-
        #-*- mode: hylangScript -*-
        /* -*-hylangScript-*- */
        // -*- hylang -*-
        /* -*- mode:hylang -*- */
        // -*- font:bar;mode:hylang -*-
        // -*- font:bar;mode:hylang;foo:bar; -*-
        // -*-font:mode;mode:hylang-*-
        // -*- foo:bar mode: hylangSCRIPT bar:baz -*-
        " -*-foo:bar;mode:hylang;bar:foo-*- ";
        " -*-font-mode:foo;mode:hylang;foo-bar:quux-*-"
        "-*-font:x;foo:bar; mode : hylang; bar:foo;foooooo:baaaaar;fo:ba;-*-";
        "-*- font:x;foo : bar ; mode : hylangScript ; bar : foo ; foooooo:baaaaar;fo:ba-*-";
      """
      for line in valid.split /\n/
        expect(grammar.firstLineRegex.scanner.findNextMatchSync(line)).not.toBeNull()

      invalid = """
        /* --*hylang-*- */
        /* -*-- hylang -*-
        /* -*- -- hylang -*-
        /* -*- hylang -;- -*-
        // -*- ihylang -*-
        // -*- hylang; -*-
        // -*- hylang-door -*-
        /* -*- model:hylang -*-
        /* -*- indent-mode:hylang -*-
        // -*- font:mode;hylang -*-
        // -*- mode: -*- hylang
        // -*- mode: das-hylang -*-
        // -*-font:mode;mode:hylang--*-
      """
      for line in invalid.split /\n/
        expect(grammar.firstLineRegex.scanner.findNextMatchSync(line)).toBeNull()

    it "recognises Vim modelines", ->
      valid = """
        vim: se filetype=hylang:
        # vim: se ft=hylang:
        # vim: set ft=hylang:
        # vim: set filetype=hylang:
        # vim: ft=hylang
        # vim: syntax=hylang
        # vim: se syntax=hylang:
        # ex: syntax=hylang
        # vim:ft=hylang
        # vim600: ft=hylang
        # vim>600: set ft=hylang:
        # vi:noai:sw=3 ts=6 ft=hylang
        # vi::::::::::noai:::::::::::: ft=hylang
        # vim:ts=4:sts=4:sw=4:noexpandtab:ft=hylang
        # vi:: noai : : : : sw   =3 ts   =6 ft  =hylang
        # vim: ts=4: pi sts=4: ft=hylang: noexpandtab: sw=4:
        # vim: ts=4 sts=4: ft=hylang noexpandtab:
        # vim:noexpandtab sts=4 ft=hylang ts=4
        # vim:noexpandtab:ft=hylang
        # vim:ts=4:sts=4 ft=hylang:noexpandtab:\x20
        # vim:noexpandtab titlestring=hi\|there\\\\ ft=hylang ts=4
      """
      for line in valid.split /\n/
        expect(grammar.firstLineRegex.scanner.findNextMatchSync(line)).not.toBeNull()

      invalid = """
        ex: se filetype=hylang:
        _vi: se filetype=hylang:
         vi: se filetype=hylang
        # vim set ft=klojure
        # vim: soft=hylang
        # vim: clean-syntax=hylang:
        # vim set ft=hylang:
        # vim: setft=hylang:
        # vim: se ft=hylang backupdir=tmp
        # vim: set ft=hylang set cmdheight=1
        # vim:noexpandtab sts:4 ft:hylang ts:4
        # vim:noexpandtab titlestring=hi\\|there\\ ft=hylang ts=4
        # vim:noexpandtab titlestring=hi\\|there\\\\\\ ft=hylang ts=4
      """
      for line in invalid.split /\n/
        expect(grammar.firstLineRegex.scanner.findNextMatchSync(line)).toBeNull()
