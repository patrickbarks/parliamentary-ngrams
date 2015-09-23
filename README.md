parliamentary n-grams
=====================

Scrape and analyze [transcripts](http://www.parl.gc.ca/HouseChamberBusiness/ChamberSittings.aspx?View=H&Parl=41&Ses=1&Language=E&Mode=2) of daily debates in the House of Commons.


### Data files

parl-41-raw.txt contains all (hope I got 'em all) 45,928 turns-of-talk from the 502 Question Periods held during the 41st Parliament

Variable | Description
---|---------
`date` | date in dd-mm-yyyy format
`name` | name of the MP speaking
`party` | party affiliation of the MP speaking
`speech` | text of the MP's speech (translated into English if initially spoken in French)

parl-41-processed.txt is the same, but with non-target turns-of-talk removed (e.g. directions from The Speaker of the House, jeers from unnamed members, etc.), missing party affiliations filled in, and MP names made consistent (some MPs are referred to in the transcripts in multiple ways, e.g. with or without their middle initial)


### Example analyses

The distribution of dollar-value mentions by party:

![](/figures/dollar-mentions.png)

The percent occurrence of various terms over time:

![](/figures/n-grams.png)