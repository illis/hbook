hbook - an address book query tool for Mutt
---


## Features
- Will take a search query & vcard file and return a mutt alias. 
- Results are sorted by the fuzzy search filter.


## Usage
```
hbook-exe --mutt-query %vcard-filename% %min-search-score% %search-param% 
```
vcard-filename - location of your vcf file
search-param - your search parameter
min-search-score - a value between 0 (lowest match) & 5 (highest match)

### Example usage with mutt
In your muttrc file:
```
set query_command = "hbook-exe -q '%s' -m 3 -f contacts.vcf"
```
Then when you hit ctrl-t when entering an address, it should pop you up with a list.


## Caveats
- Originally written to learn some haskell and play around with megaparsec & hspec. Constructive criticism is welcome.
- Not currently a complete vcard implementation. 
- Willing to make tweaks if it doesnt work for you


## References 
vcard rfc: https://tools.ietf.org/html/rfc6350
address book format for mutt: https://dev.mutt.org/trac/wiki/MuttGuide/Aliases
