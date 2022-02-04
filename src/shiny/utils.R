# Item 1 - 8 descriptions taken from wikipedia, 9+ from SEC
items_full_def <- "
Item 1 - Business: This describes the business of the company; who and what the company does, what subsidiaries it owns, and what markets it operates in. It may also include recent events, competition, regulations, and labor issues. (Some industries are heavily regulated, have complex labor requirements, which have significant effects on the business.) Other topics in this section may include special operating costs, seasonal factors, or insurance matters.
Item 1A - Risk Factors: Here, the company lays anything that could go wrong, likely external effects, possible future failures to meet obligations, and other risks disclosed to adequately warn investors and potential investors.
Item 1B - Unresolved Staff Comments: Requires the company to explain certain comments it has received from the SEC staff on previously filed reports that have not been resolved after an extended period of time. Check here to see whether the SEC has raised any questions about the company’s statements that have not been resolved.
Item 2 - Properties: This section lays out the significant properties, physical assets, of the company. This only includes physical types of property, not intellectual or intangible property.
Item 3 - Legal Proceedings: Here, the company discloses any significant pending lawsuit or other legal proceeding. References to these proceedings could also be disclosed in the Risks section or other parts of the report.
Item 4 - Mine Safety Disclosures: This section requires some companies to provide information about mine safety violations or other regulatory matters.
Item 5 - Market: Gives highs and lows of stock, in a simple statement. Market for Registrant's Common Equity, related stockholder matters and issuer purchases of equity securities.
Item 6 - Consolidated Financial Data: In this section Financial Data showing consolidated records for the legal entity as well as subsidiary companies.
Item 7 - Management's Discussion and Analysis of Financial Condition and Results of Operations: Here, management discusses the operations of the company in detail by usually comparing the current period versus prior period. These comparisons provide a reader an overview of the operational issues of what causes such increases or decreases in the business. MDA MD&A
Item 7A - Quantitative and Qualitative Disclosures about Market Risks: Forward Looking Statements, Forward-looking statement is the disclaimer that projections as to future performance are not guaranteed, and things could go otherwise. FLS
Item 8 - Financial Statements: Independent Auditor's Report, Consolidated Statements of Operation, Consolidated Balance Sheets, Other accounting reports and notes. Here, also, is the going concern opinion. This is the opinion of the auditor as to the viability of the company. Look for 'unqualified opinion' expressed by auditor. This means the auditor had no hesitations or reservations about the state of the company, and the opinion is without any qualifications (unconditional).
Item 9 - Changes in and Disagreements With Accountants on Accounting and Financial Disclosure: If there has been a change in its accountants, discussion re any disagreements it had with those accountants.
Item 9A - Controls and Procedures: Information about the company’s disclosure controls and procedures and its internal control over financial reporting.
Item 9B - Other Information: Includes any information that was required to be reported on a Form 8-K during the fourth quarter of the year covered by the 10-K, but was not yet reported.
Item 10 - Directors, Executive Officers and Corporate Governance: Background and experience of the company’s directors and executive officers, the company’s code of ethics, and certain qualifications for directors and committees of the board of directors.
Item 11 - Executive Compensation: Detailed disclosure about the company’s compensation policies and programs and how much compensation was paid to the top executive officers of the company in the past year.
Item 12 - Security Ownership of Certain Beneficial Owners and Management and Related Stockholder Matters: Information about the shares owned by the company’s directors, officers and certain large shareholders, and about shares covered by equity compensation plans.
Item 13 - Certain Relationships and Related Transactions, and Director Independence: Relationships and transactions between the company and its directors, officers and their family members. It also includes information about whether each director of the company is independent.
Item 14 - Principal Accounting Fees and Services: Disclosure of fees paid to accounting firm for various types of services during the year, often supplied in separate proxy statement.
Item 15 - Exhibits, Financial Statement Schedules Signatures: List of the financial statements and exhibits included as part of the Form 10-K.
"
item_defs  <- c("(top): top", stringi::stri_split_lines(items_full_def, omit_empty = TRUE)[[1]])
df_item_defs <- tibble(item_defs) %>%
  tidyr::separate(item_defs, c("display_label", "description"), ": ") %>%
  mutate(item_name = sapply(str_split(display_label, " - "), "[", 1)) %>%
  mutate(item_id = str_replace(str_to_lower(item_name), " ", "")) %>%
  mutate(is_available = TRUE)

entity_type_full_def <- "
PERSON: People, including fictional
NORP: Nationalities or religious or political groups
FACILITY: Buildings, airports, highways, bridges, etc.
ORG: Companies, agencies, institutions, etc.
GPE: Countries, cities, states
LOCATION: Non-GPE locations, mountain ranges, bodies of water
PRODUCT: Vehicles, weapons, foods, etc. (Not services)
EVENT: Named hurricanes, battles, wars, sports events, etc.
WORK OF ART: Titles of books, songs, etc.
LAW: Named documents made into laws
LANGUAGE: Any named language
DATE: Absolute or relative dates or periods
TIME: Times smaller than a day
PERCENT: Percentage (including '%')
MONEY: Monetary values, including unit
QUANTITY: Measurements, as of weight or distance
ORDINAL: 'first', 'second'
CARDINAL: Numerals that do not fall under another type
"
entity_defs  <- stringi::stri_split_lines(entity_type_full_def, omit_empty = TRUE)[[1]]
df_entity_defs <- tibble(entity_defs) %>%
  tidyr::separate(entity_defs, c("display_label", "description"), ": ")
df_entity_defs <- bind_cols(df_entity_defs[1:9, ], df_entity_defs[10:18, ])

esg_type_keywords <- "
Environmental: Biodiversity, Carbon, Cleantech, Clean, Climate, Coal, Conservation, Ecosystem, Emission, Energy, Fuel, Green, Land, Natural, Pollution, Raw Materials, Renewable, Resources, Sustainability, Sustainable, Toxic, Waste, Water
Social: Accident, Adult Entertainment, Alcohol, Anti-personnel, Behavior, Charity, Child Labor, Community, Controversial, Controversy,  iscrimination, Gambling, Health, Human capital, Human rights, Inclusion, Injury, Labor, Munitions, Opposition, Pay, Philanthropic, Quality, Responsible
Governance: Advocacy, Bribery, Compensation, Competitive, Corruption, Data Breach, Divestment, Fraud, Global Compact, GRI, Global Reporting Initiative, Independent, Justice, Stability, Stewardship, Transparency
"
esg_defs <- stringi::stri_split_lines(esg_type_keywords, omit_empty = TRUE)[[1]]
df_esg_defs <- tibble(esg_defs) %>%
  tidyr::separate(esg_defs, c("category", "marked keywords"), ": ")

# return idx of all columns except those in keep, subtract 1 for DT indexes
dt_idx_to_hide <- function(seq_obj, keep) {
  all_idx <- seq_along(1:length(names(seq_obj)))
  return(setdiff(all_idx,  match(keep, names(seq_obj))) - 1)
}

dt_idx_col_names <- function(dt_names, col_names) {
  return(which(dt_names %in% col_names) - 1)
}
