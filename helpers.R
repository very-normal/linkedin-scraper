collectJobDescriptionItems = function() {
  
  # Isolate the job details div element based on currently shown posting
  ul_elems = tryCatch({
    remDr$findElements(using = "xpath", "//div[@id='job-details']//span//ul")
  }, error = function(e) {
    NULL  # return NULL if an error occurs
  })
  
  # If there were no <ul> elements, then return a missing value
  if (is.null(ul_elems)) { return(NA) } 
  
  ul_list = vector("list", length = length(ul_elems))
  
  for (i in seq_along(ul_elems)) {

    li_elems = ul_elems[[i]]$findElements(using = "xpath", ".//li")
    
    # Store the vector of li texts in the list
    ul_list[[i]] = sapply(li_elems, function(li) { li$getElementText() }) %>% 
      unique %>% .[[1]] %>% str_split_1("\n") 
  }
  
  ul_list %>% unlist()
  
}

scrapePostingsForKeyword = function(keyword, username, password, start,
                                    maxpages = NULL, verbose = T) {
  
  ##############################################################################
  
  # GETTING INTO LINKEDIN
  
  ##############################################################################
  
  if (verbose) {
    print(paste0("Starting up scraper: ", lubridate::now()))
    print(paste0("Logging in with user details: ", lubridate::now()))
  }
  
  # Navigate to LinkedIn
  remDr$navigate("https://www.linkedin.com/jobs")
  
  # Get username and password input boxes
  user = remDr$findElement(using = "xpath", '//*[@id="session_key"]')
  pw = remDr$findElement(using = "xpath", '//*[@id="session_password"]')
  
  # Input the email id and password
  user$sendKeysToElement(list(username))
  pw$sendKeysToElement(list(password))
  
  # Select the login button and log in
  login_btn = remDr$findElement(using = "css selector", 'button[data-id="sign-in-form__submit-btn"]')
  login_btn$clickElement()
  
  if (verbose) {
    print(paste0("Done logging in: ", lubridate::now()))
  }
  
  ##############################################################################
  
  # SEARCHING & SCRAPING
  
  ##############################################################################
  
  if (verbose) {
    print(paste0("Beginning web scraping of posts: ", lubridate::now()))
  }
  
  # Enforce that the keyword is in the job title
  strict_keyword = paste0('\"', keyword, '\"')
  search = remDr$findElement(using = "css selector", 'input.jobs-search-box__text-input')
  search$sendKeysToElement(list(strict_keyword, key = "enter"))
  
  # Waiting for the page to load so that the page number elements will appear
  Sys.sleep(2)
  
  # Target the pagination at the bottom of the page
  # Then, find out how many pages of results to go through
  pages = remDr$findElements(using = "css selector", 'ul.artdeco-pagination__pages li')
  npages = pages[[length(pages)]]$getElementText() %>% unlist() %>% as.numeric
  
  if (verbose) {
    print(paste0("Number of pages to scrape: ", npages, ", ", lubridate::now()))
  }
  
  # Use either the number of postings or the given max
  if (!is.null(maxpages))
    npages = ifelse(npages > maxpages, maxpages, npages)
  
  data_list = vector("list", length = npages) 
  
  # For each page, go through each of the job postings and collect information
  # on responsibilities and skills
  for (i in start:npages) {
    
    if (verbose) {
      print(paste0("Scraping page ", i, ": ", lubridate::now()))
    }
    
    current_pages = remDr$findElements(using = "css selector", 'ul.artdeco-pagination__pages li')
    currently_shown_page_numbers = current_pages %>% 
      map(function(li) { li$getElementText() }) %>% 
      unlist()
    
    # Covering a weird edge case where the list doesn't show 9 at the 8th page
    if (i == 9) {
      current_pages[[i]]$clickElement()
    } else {
      position_in_pagination = which(currently_shown_page_numbers == as.character(i))
      current_pages[[position_in_pagination]]$clickElement()
    }
    
    # Waiting for number of job posting pages to appear
    Sys.sleep(2)
    
    # Select all the list of all job postings shown on current page
    postings = remDr$findElements(using = "css selector", '.jobs-search-results-list ul.scaffold-layout__list-container li.jobs-search-results__list-item')
    npostings = postings %>% length
    
    page_list = vector("list", length = npostings) 
    
    for (j in 1:npostings) {
      
      # Click on the posting to show the job description
      postings[[j]]$clickElement()
      
      # Waiting for the page to load long enough so entire job description will appear
      Sys.sleep(5)
      
      # Get various job and company information & format as strings
      job_title = remDr$findElement(using = "css", "h2.jobs-unified-top-card__job-title")$getElementText() %>% unlist
      company_name = remDr$findElement(using = "css selector", ".jobs-unified-top-card__company-name")$getElementText() %>% unlist
      company_location = remDr$findElement(using = "css selector", ".jobs-unified-top-card__bullet")$getElementText() %>% unlist
      company_workplace_type = tryCatch({
        remDr$findElement(using = "css selector", ".jobs-unified-top-card__workplace-type")$getElementText() %>% unlist
      }, error = function(e) {
        NA  # return NULL if an error occurs
      })
      
      # Click on the "Show all skills" button to see desired skills
      # NOTE: If your bot window is too small, the pop-up won't appear
      skills_button = remDr$findElement(using = 'xpath', '//button[.//span[text()="Show all skills"]]')
      skills_button$clickElement()
      
      # Waiting for number of job posting pages to appear
      Sys.sleep(2)
      
      # Now collect all of the skills that were shown
      matched = remDr$findElements(using = "css selector", "ul.job-details-skill-match-status-list li.job-details-skill-match-status-list__matched-skill")
      unmatched = remDr$findElements(using = "css selector", "ul.job-details-skill-match-status-list li.job-details-skill-match-status-list__unmatched-skill")
      
      skills = c(
        sapply(matched, function(skill) skill$getElementText()) %>% unlist,
        sapply(unmatched, function(skill) skill$getElementText()) %>% unlist %>% str_replace("\nAdd", "") # Removes text from add button
      )
      
      # Select button based on the fact that it contains the string "Done" 
      # And click it to remove the pop-up
      skills_done_button = remDr$findElement(using = 'xpath', '//button[.//span[text()="Done"]]')
      skills_done_button$clickElement()
      
      # Collect all of the li items in the job description
      # this takes a few seconds, so this helps throttle the scraping
      bullets = collectJobDescriptionItems()
      
      page_list[[j]] = tibble(
        job_title = job_title,
        company_name = company_name, 
        company_location = company_location, 
        company_workplace_type = company_workplace_type,
        skills = list(skills),
        bullets = list(bullets)
      )

    } # end of loop over postings
    
    # Saving into data folder early as a check
    saveRDS(bind_rows(page_list), paste0("./data/2023-05-29-biostatistician-scrape-page-", i ,".rds"))
    
  } # end of loop over pages
  
}
