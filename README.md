<p align="center">
  <img src="res/logo2.png" alt="EDGAugmentor logo" width="20%">
</p>

# EDGAugmentor

## the Shiny web app bit:

</br>

### IRL  
http://owenG.net/EDGAugmentor/

</br>

### words

Project began with the idea of doing 'NLP stuff' based on text from specific Items within 10-k filings. Which rather quickly brought up the issue of identifying the begin/end of each Item and extracting relevant text. Which rather slowly led to many iterations of regex experimentations coinciding with the idea of keeping the relatively pretty HTML formatting (and charts, tables etc.) of a given 10-k while also doing 'NLP stuff' on the underlying text as opportunity presented. That seemed to mostly work but was difficult to QA, i.e. to know that a given extraction had correctly identified the full text for that Item. Highlighting the sections via CSS worked well enough on one-off situations but it seemed something more app-like would be helpful, and so was born the Shiny/R web app now known as `EDGAugmentor`. At this point in time EDGAugmentor's theoretical first-purpose is to help in the human review of 10-k filed with the SEC.

The initial pool of 10-k were semi-randomly (favoring relatively small file sizes) pulled down from EDGAR a while back and run through a series of steps:
* extract only the actual 10-k HTML from the full report filing
* join with some ticker data
* run the 10-k HTML through a series of Item-specific regex patterns, with logic to locate safest point at which inject `<section>` tags, e.g. in some cases it is necessary to find parent `<table>` element in order not to break HTML structure. Store some metadata in a .json file, e.g. some filings were missing certain item sections, either because item was really not present or the available marker text wasn't a good enough match for its defined regex
* separately enhance above metadata file with additional metadata, e.g. address info pulled from the EDGAR filing page.
* for a given chunk of HTML section text, initially Item 7 for named entity recognition (NER) work, run it's text-only equivalent through basic English "web" spaCy model&#8224; in order to identify 'named entities'. Locate versions of same in the orig HTML version and surround with `<mark>` tags specific to entity type (and hope for the best)
* as a simple follow-on to above, the text for each Item 1 was run through a very simple series of ESG related keywords. Nothing model-based etc., only a find-and-flag on relatively narrow universe of 60+ words categorized into one of Environmental/Social/Governance categories.

With all of above text pre-processing in place the sample set of 10-k are available for Item-by-Item navigation, background shading provided to differentiate Item sections from one to the next. 
Additionally, two of the Items have the option of highlighting certain text, activated by choosing to "&#9745; Show marked" and then ensuring one or more of the displayed categories are checked:
* Item 1 optionally highlights ESG related keywords
* Item 7 optionally highlights entities belonging to one of the standard types identified by spaCy's NER pipeline 
</br>
</br>
---

### LLM arrives
Why try to read through pages and pages of dense financial prose when ChatGPT can condense the key details in clear
and succinct wording? Here you have how one insightful summary begins:
> 	Well, it turns out that our regulatory nightmare is just the tip of the iceberg. 

</br>

June, 2023 finds the project hungry for more NLP, this time in the form of large language models. Happily there is a
thing known as ChatGPT, with a straightforward API provided by OpenAI. Just as importantly `langchain` is on the scene - 
exponential backoff you say? Let `langchain` deal with that me say. After ~~extenstive testing and evaluation of 
respective outputs~~ checking the per-token 
costs of the various LLM models available through the OpenAPI, `gpt-3.5-turbo` was selected. Take some Item 1 raw
text, push into `load_summarize_chain()` along with prompts requesting either a basic summary or one in
"snarky style" (whilst using great restraint to not also ask for a "very snarky style" summary), dross is expunged and only gold remains. Add a smidge of spaCy to break up any longer text blobs into chunks of sentences and the new update
is ready.  
</br>

#### *Coming unsoon*
 Maybe some ideas from recent University of Chicago [Bloated Disclosures: Can ChatGPT Help Investors Process Information?](https://bfi.uchicago.edu/working-paper/bloated-disclosures-can-chatgpt-help-investors-process-information/) paper. Maybe live Q&A on the MD&A section text.


</br>
</br>
&#8224; turns out default spaCy models, e.g. `en_core_web_md` , do NOT produce very good NER results when passed typical 10-k text, who knew there were so many `Work Of Art`s referenced there :). A better model is on the future-thing list.  

</br>

---
</br>
</br>
</br>

### general browsing

</br>

<p align="center">
  <img src="res/browse_quick.gif" alt="Item 7 text with NER highlighting" width="70%">
</p>
</br>

---

</br>

### named entity highlighting

</br>

<p align="center">
  <img src="res/ner.gif" alt="Item 7 text with NER highlighting" width="70%">
</p>

