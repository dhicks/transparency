- identifying columns:
    - IPAddress, RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitutde, LocationLongitude, PROLIFIC_PID

- assertions
    - All respondents gave consent
    - After filtering by Progress >= 99, no duplicates by PROLIFIC_PID

- consolidate METI responses

- use Progress >= 99 to check completion

- Match to Prolific data file using 'PROLIFIC_PID' = 'participant_id'

- 21 pilot responses
    - all on Oct 1
    - don't match with Prolific metadata
    
- 1006 unique, completed responses from main run

- thinking of using 3 gender categories: GC (gender-conforming) woman, GC man, gender non-conforming (GNC)
    - NA-NA for gender questions drop out when filtering by Progress


