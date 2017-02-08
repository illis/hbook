
## MuttRC entry for abook
```
set query_command= "abook --mutt-query '%s'"
macro index,pager  a "<pipe-message>abook --add-email-quiet<return>" "Add this sender to Abook"
bind editor        <Tab> complete-query
```

## Query expected from mutt
```
<email address> <tab> <long name> <tab> <other info> <newline> 
```

## VCard specification
https://tools.ietf.org/html/rfc6350
 
