print ("hi")
sony <- read.csv("/var/www/sony-report/import/sony2.csv", header=TRUE)
print(is.data.frame(sony))
print(ncol(sony))
print(nrow(sony))

fail <- list("Failed Labels")

for(i in names(sony)){
  bundle <- substring(i, 0, regexpr("_",i)-1)
  name <- substring(i, regexpr("_",i)+1)

  if(i == "Single_Age"){
    if(class(sony[[i]]) != "integer"){
      fail[length(fail) + 1] <- i
    }
  }else if(i == "Gender"){
    if(!("Male" %in% sony[[i]] | "Female" %in% sony[[i]])){
      fail[length(fail) + 1] <- i
    }
  }else if(bundle == "Household"){
    if(name == "Income"){
      if(!("Less than $25,000" %in% sony[[i]] |
           "Don't know" %in% sony[[i]] |
           "Between $75,000 and $99,000" %in% sony[[i]] |
           "Between $100,000 and $250,000" %in% sony[[i]] |
           "Between $75,000 and $99,000" %in% sony[[i]] |
           "Between $45,000 and $74,000" %in% sony[[i]] |
           "More than $250,000" %in% sony[[i]] |
           "Between $25,000 and $44,000" %in% sony[[i]] |
           "Prefer not to say" %in% sony[[i]])){
        fail[length(fail) + 1] <- i
      }
    } else if(name == "Makeup") {
      if(!(
        "I am a single parent, living with my kid(s)" %in% sony[[i]]|
        "I live alone" %in% sony[[i]]|
        "I live in a multi-generational household" %in% sony[[i]]|
        "I live with friend(s) / roommate(s)" %in% sony[[i]]|
        "I live with my parent(s)" %in% sony[[i]]|
        "I live with my partner and our kid(s)" %in% sony[[i]]|
        "I live with my partner, but our kid(s) have left home" %in% sony[[i]]|
        "I live with my partner, but we don't have any kids" %in% sony[[i]]|
        "Other" %in% sony[[i]]
      )){
        fail[length(fail) + 1] <- i
      }
    } else {
      if(!("True" %in% sony[[i]] | "False" %in% sony[[i]])){
        fail[length(fail) + 1] <- i
      }
    }
  }
  else if(bundle %in% c('PrimetimeTV',
                        'Fitness',
                        'Social',
                        'Soda',
                        'Music',
                        'Mobile',
                        'Toy',
                        'Finance',
                        'Speakers',
                        'SpanishTV',
                        'Like',
                        'Car',
                        'TV',
                        'MusicWeb',
                        'MiscSubscription',
                        'Beer',
                        'Newspapers',
                        'FreeStreaming',
                        'Electronics',
                        'Luxury',
                        'Magazines',
                        'Clothing',
                        'Food',
                        'PersonalCare',
                        'Household',
                        'Airline',
                        'ActiveWear',
                        'MusicTV',
                        'PremiumTV',
                        'PaidStreaming',
                        'CableTV',
                        'Restaurant',
                        'MobileApps',
                        'ThemeParks',
                        'MobileMusic',
                        'Web',
                        'Retailer',
                        'DaytimeTV',
                        'Beverage',
                        'Provider',
                        'LateTV',
                        'Speakers/Headphones',
                        'MusicPubs',
                        'Blogs',
                        'Online',
                        'Primary',
                        'BigFan',
                        'Hotel',
                        'Cruise',
                        'Alcohol',
                        'Concerts',
                        'Sports',
                        'Awards',
                        'Live',
                        'Genre'
  )){
    if(!("True" %in% sony[[i]] | "False" %in% sony[[i]])){
      fail[length(fail) + 1] <- i
    }
  } else if(i == 'WEIGHT'){
    if(class(sony[[i]]) != "numeric"){
      fail[length(fail) + 1] <- i
    }
  }else if(bundle == 'FECI'){
    if(name == 'Score'){
      if(class(sony[[i]]) != "integer"){
        fail[length(fail) + 1] <- i
      }
    }else if(name == 'GROUP'){
      if(
        !("Enthusiast" %in% sony[[i]]|
          "Indifferent" %in% sony[[i]]|
          "Casual" %in% sony[[i]]|
          "Fanatic" %in% sony[[i]]
        )
      ){
        fail[length(fail) + 1] <- i
      }
    }
  }else if(i == 'Sexual_Orientation'){
    if('None of the above' %in% sony[[i]] |
      'Transsexual/Intersex' %in% sony[[i]] |
      'Heterosexual/Straight' %in% sony[[i]] |
      'Gay/Lesbian' %in% sony[[i]] |
      'Prefer not to answer' %in% sony[[i]] |
      'Transgender' %in% sony[[i]] |
      'Bisexual' %in% sony[[i]]
){
      fail[length(fail) + 1] <- i
    }
  }else if(bundle %in% c("Latino", "Country")){
    if(!("Selected" %in% sony[[i]] | "Not Selected" %in% sony[[i]])){
      fail[length(fail) + 1] <- i
    }
  }else if(bundle == 'Latin'){
    if(name == 'Ethnicity'){
      if(class(sony[[i]]) != "integer"){
        fail[length(fail) + 1] <- i
      }
    }else{
      if(!("True" %in% sony[[i]] | "False" %in% sony[[i]])){
        fail[length(fail) + 1] <- i
      }
    }
  }else if(bundle=="Race"){
    if(!(name %in% c('Detailed','Simple'))){
      if(!("True" %in% sony[[i]] | "False" %in% sony[[i]])){
        fail[length(fail) + 1] <- i
      }
    }
  }else if(bundle == 'Occupation'){
    if(!('Working part time' %in% sony[[i]] |
      'Prefer not to answer' %in% sony[[i]] |
      'Retired' %in% sony[[i]] |
      'Other' %in% sony[[i]] |
      'Currently looking for employment' %in% sony[[i]] |
      'Part time student' %in% sony[[i]] |
      'Full time student' %in% sony[[i]] |
      'Working full time' %in% sony[[i]]
      )){
        fail[length(fail) + 1] <- i
      }
  }else if(bundle == 'Age'){
    if(name == 'Cohorts'){
      if(!('60+' %in% sony[[i]] |
      '18-21' %in% sony[[i]] |
      '22-25' %in% sony[[i]] |
      '13-17' %in% sony[[i]] |
      '35-39' %in% sony[[i]] |
      '26-29' %in% sony[[i]] |
      '50-54' %in% sony[[i]] |
      '55-59' %in% sony[[i]] |
      '30-34' %in% sony[[i]] |
      '40-44' %in% sony[[i]] |
      '45-49' %in% sony[[i]])){
        fail[length(fail) + 1] <- i
      }
    }else if(name == 'by_Gender'){
      if(!('60+' %in% sony[[i]] |
        '18-21' %in% sony[[i]] |
        '22-25' %in% sony[[i]] |
        '13-17' %in% sony[[i]] |
        '35-39' %in% sony[[i]] |
        '26-29' %in% sony[[i]] |
        '50-54' %in% sony[[i]] |
        '55-59' %in% sony[[i]] |
        '30-34' %in% sony[[i]] |
        '40-44' %in% sony[[i]] |
        '45-49' %in% sony[[i]])){
          fail[length(fail) + 1] <- i
        }
    }else{
      print(i)
    }
  }else if(i == 'Political_Affiliation'){
    if(!('Democratic' %in% sony[[i]] |
    'Other' %in% sony[[i]] |
    'Green' %in% sony[[i]] |
    'Socialist' %in% sony[[i]] |
    'Independent' %in% sony[[i]] |
    'None of the above' %in% sony[[i]] |
    'Constitution' %in% sony[[i]] |
    'Republican' %in% sony[[i]] |
    'Prefer not to answer' %in% sony[[i]] |
    'Libertarian' %in% sony[[i]])){
      fail[length(fail) + 1] <- i
    }
  }else if(bundle == 'Tactic'){
    if(!('No, I dislike it' %in% sony[[i]] |
  'Yes, I like it' %in% sony[[i]] |
  'Neither like nor dislike' %in% sony[[i]])){
      fail[length(fail) + 1] <- i
    }
  }else{
    print(i)
  }
}
