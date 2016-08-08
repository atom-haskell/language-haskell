module.exports =
  [
    {
      name: 'block.liquidhaskell'
      contentName: 'block.liquidhaskell.annotation'
      begin: '\\{-@(?!#)'
      end: '@-\\}'
      patterns: [
          include: '$self'
      ]
    }
    {
      name: 'comment.line.shebang.haskell'
      match: '^\\#\\!.*\\brunhaskell\\b.*$'
    }
    { include: '#infix_function' }
    { include: '#unit' }
    { include: '#empty_list' }
    { include: '#quasi_quotes' }
    { include: '#module_decl' }
    { include: '#class_decl' }
    { include: '#instance_decl' }
    { include: '#foreign_import' }
    { include: '#regular_import' }
    { include: '#data_decl' }
    { include: '#type_alias' } # TODO: review stopped here
    { include: '#keywords' }
    { include: '#c_preprocessor' }
    { include: '#pragma' }
    { include: '#string' }
    { include: '#newline_escape' }
    { include: '#quoted_character' }
    { include: '#scoped_type_override' }
    { include: '#function_type_declaration' }
    { include: '#scoped_type' }
    { include: '#prelude' }
    { include: '#comments' }
    { include: '#infix_op' }
    { include: '#comma' }
    { include: '#lit_num' }
    { include: '#operator' }
    { include: '#identifier' }
    { include: '#type_ctor' }
  ]
