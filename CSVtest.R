print ("Scanning")
sony <- read.csv("/var/www/sony-report/import/sony2.csv", header=TRUE)
oldlabels <- read.csv("~/Documents/R_Project/sony/correct_spelling_list.csv", header = FALSE)
print(is.data.frame(sony))
print(ncol(sony))
print(nrow(sony))

fail <- data.frame()
newbundle <- data.frame()

if(!('Single_Age' %in% names(sony))){
  newfail <- data.frame(
    label = 'Single_Age'
  )
  fail <- rbind(fail,newfail)
}
if(!('Gender' %in% names(sony))){
  newfail <- data.frame(
    label = 'Gender'
  )
  fail <- rbind(fail,newfail)
}
if(!('State' %in% names(sony))){
  newfail <- data.frame(
    label = 'State'
  )
  fail <- rbind(fail,newfail)
}
if(!('Household_Income' %in% names(sony))){
  newfail <- data.frame(
    label = 'Household_Income'
  )
  fail <- rbind(fail,newfail)
}
if(!('Household_Makeup' %in% names(sony))){
  newfail <- data.frame(
    label = 'Household_Makeup'
  )
  fail <- rbind(fail,newfail)
}

for(i in names(sony)){
  if(i %in% c("RespondentID", "OrderID", "ResStatus", "StartDate")){
    next
  }
  bundle <- substring(i, 0, regexpr("_",i)-1)
  name <- substring(i, regexpr("_",i)+1)

  if(i == "Single_Age"){
    if(class(sony[[i]]) != "integer"){
      newfail <- data.frame(
        label = i
      )
      fail <- rbind(fail,newfail)
    }
  }else if(i == "Gender"){
    if(!("Male" %in% sony[[i]] | "Female" %in% sony[[i]])){
      newfail <- data.frame(
        label = i
      )
      fail <- rbind(fail,newfail)
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
        newfail <- data.frame(
          label = i
        )
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
        newfail <- data.frame(
          label = i
        )
      }
    } else {
      if(!("True" %in% sony[[i]] | "False" %in% sony[[i]])){
        newfail <- data.frame(
          label = i
        )
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
                        'MusicDiscovery',
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
                        'BigFan',
                        'Hotel',
                        'Cruise',
                        'Alcohol',
                        'Concerts',
                        'Sports',
                        'Awards',
                        'Live',
                        'Genre',
                        'KidsHousehold',
                        'ArtistMerch',
                        'Purchased',
                        'Consumption',
                        'Mood',
                        'Hobbies',
                        'Bonus',
                        'Devices'
  )){
    if(!("True" %in% sony[[i]] | "False" %in% sony[[i]])){
      newfail <- data.frame(
        label = i
      )
      fail <- rbind(fail,newfail)
    }

    if(!(i %in% oldlabels[['V1']]) & !(bundle %in% c('KidsHousehold', 'Mood'))){
      newbundleRow <- data.frame(
        New_Labels = i
      )

      newbundle <- rbind(newbundle,newbundleRow)
    }
  } else if(i == 'WEIGHT'){
    if(class(sony[[i]]) != "numeric"){
      newfail <- data.frame(
        label = i
      )
      fail <- rbind(fail,newfail)
    }
  }else if(bundle == 'FECI'){
    if(name == 'Score'){
      if(class(sony[[i]]) != "integer"){
        newfail <- data.frame(
          label = i
        )
        fail <- rbind(fail,newfail)

      }
    }else if(name %in% c('GROUP_FINAL', 'COUNTRY_FINAL', 'HISPANIC_FINAL')){
      if(class(sony[[i]]) == 'factor'){
        newfail <- data.frame(
          label = i
        )
        fail <- rbind(fail,newfail)

      }
    }else if(name == 'GROUP_AGE'){
      if("Indifferents 50+" %in% sony[[i]] |
"Indifferents 26-39" %in% sony[[i]] |
"Casuals 35-49" %in% sony[[i]] |
"Enthusiasts 13-17" %in% sony[[i]] |
"Causals 18-25" %in% sony[[i]] |
"Indifferents 40-49" %in% sony[[i]] |
"Enthusiasts 50+" %in% sony[[i]] |
"Fanatics 18-25" %in% sony[[i]] |
"Fanatics 55+" %in% sony[[i]] |
"Enthusiasts 30-39" %in% sony[[i]] |
"Fanatics 26-34" %in% sony[[i]] |
"Enthusiasts 18-21" %in% sony[[i]] |
"Indifferents 13-25" %in% sony[[i]] |
"Fanatics 35-44" %in% sony[[i]] |
"Enthusiasts 22-29" %in% sony[[i]] |
"Enthusiasts 40-49" %in% sony[[i]] |
"Casuals 26-34" %in% sony[[i]] |
"Casuals 13-17" %in% sony[[i]] |
"Fanatics 45-54" %in% sony[[i]] |
"Causals 50+" %in% sony[[i]] |
"Fanatics 13-17" %in% sony[[i]]){
        newfail <- data.frame(
          label = i
        )
        fail <- rbind(fail,newfail)
      }
    }else if(name == 'GROUP'){
      if(
        !("Enthusiast" %in% sony[[i]]|
          "Indifferent" %in% sony[[i]]|
          "Casual" %in% sony[[i]]|
          "Fanatic" %in% sony[[i]]
        )
      ){
        newfail <- data.frame(
          label = i
        )
        fail <- rbind(fail,newfail)

      }
    }else{
      newbundleRow <- data.frame(
        New_Labels = i
      )

      newbundle <- rbind(newbundle,newbundleRow)
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
      newfail <- data.frame(
        label = i
      )
      fail <- rbind(fail,newfail)
    }
  }else if(bundle %in% c("Latino", "Country")){
    if(name == 'Filter' | name == 'filter'){
      if(!("Selected" %in% sony[[i]] | "Not Selected" %in% sony[[i]])){
        newfail <- data.frame(
          label = i
        )
        fail <- rbind(fail,newfail)
      }
    }else{
      newbundleRow <- data.frame(
        New_Labels = i
      )

      newbundle <- rbind(newbundle,newbundleRow)
    }
  }else if(bundle == 'Latin'){
    if(name == 'Ethnicity'){
      if(class(sony[[i]]) != "integer"){
        newfail <- data.frame(
          label = i
        )
      }
    }else{
      if(!("True" %in% sony[[i]] | "False" %in% sony[[i]])){
        newfail <- data.frame(
          label = i
        )
      }
    }
  }else if(bundle=="Race"){
    if(!(name %in% c('Detailed','Simple'))){
      if(!("True" %in% sony[[i]] | "False" %in% sony[[i]])){
        newfail <- data.frame(
          label = i
        )
      }
    }
  }else if(bundle == 'Occupation'){
    if(name == 'Status'){
      if(!('Working part time' %in% sony[[i]] |
        'Prefer not to answer' %in% sony[[i]] |
        'Retired' %in% sony[[i]] |
        'Other' %in% sony[[i]] |
        'Currently looking for employment' %in% sony[[i]] |
        'Part time student' %in% sony[[i]] |
        'Full time student' %in% sony[[i]] |
        'Working full time' %in% sony[[i]]
        )){
          newfail <- data.frame(
            label = i
          )
        }
    }else{
      newbundleRow <- data.frame(
        New_Labels = i
      )

      newbundle <- rbind(newbundle,newbundleRow)
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
        newfail <- data.frame(
          label = i
        )
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
          newfail <- data.frame(
            label = i
          )
        }
    }else{
      newbundleRow <- data.frame(
        New_Labels = i
      )

      newbundle <- rbind(newbundle,newbundleRow)
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
      newfail <- data.frame(
        label = i
      )
      fail <- rbind(fail,newfail)
    }
  }else if(bundle == 'Tactic'){
    if(!('No, I dislike it' %in% sony[[i]] |
  'Yes, I like it' %in% sony[[i]] |
  'Neither like nor dislike' %in% sony[[i]])){
      newfail <- data.frame(
        label = i
      )
      fail <- rbind(fail,newfail)
    }
  }else if(i == 'Living_In_US'){
    if(!('Less than 1 year' %in% sony[[i]] |
    '10+ years' %in% sony[[i]] |
    'Prefer not to answer' %in% sony[[i]] |
    'Born here' %in% sony[[i]] |
    '5 to 10 years' %in% sony[[i]] |
    '1 to 5 years' %in% sony[[i]])){
      newfail <- data.frame(
        label = i
      )
      fail <- rbind(fail,newfail)
    }
  }else if(bundle == 'Attitude'){
      if(!('Neither Agree Nor Disagree' %in% sony[[i]] |
  'Tend to Disagree' %in% sony[[i]] |
  'Strongly Disagree' %in% sony[[i]] |
  'Tend to Agree' %in% sony[[i]] |
  'Strongly Agree' %in% sony[[i]])){
        newfail <- data.frame(
          label = i
        )
        fail <- rbind(fail,newfail)
      }
  }else if(i == 'State'){
    if(!('Idaho' %in% sony[[i]] |
      'California' %in% sony[[i]] |
      'Rhode Island' %in% sony[[i]] |
      'Minnesota' %in% sony[[i]] |
      'New Jersey' %in% sony[[i]] |
      'Wyoming' %in% sony[[i]] |
      'Utah' %in% sony[[i]] |
      'New Mexico' %in% sony[[i]] |
      'Michigan' %in% sony[[i]] |
      'Mississippi' %in% sony[[i]] |
      'Hawaii' %in% sony[[i]] |
      'Kansas' %in% sony[[i]] |
      'Louisiana' %in% sony[[i]] |
      'Nebraska' %in% sony[[i]] |
      'North Dakota' %in% sony[[i]] |
      'Georgia' %in% sony[[i]] |
      'New Hampshire' %in% sony[[i]] |
      'Pennsylvania' %in% sony[[i]] |
      'South Carolina' %in% sony[[i]] |
      'Washington' %in% sony[[i]] |
      'Wisconsin' %in% sony[[i]] |
      'Florida' %in% sony[[i]] |
      'Oregon' %in% sony[[i]] |
      'West Virginia' %in% sony[[i]] |
      'Washington, D.C' %in% sony[[i]] |
      'Arizona' %in% sony[[i]] |
      'Ohio' %in% sony[[i]] |
      'Rhode Island' %in% sony[[i]] |
      'Delaware' %in% sony[[i]] |
      'Illinois' %in% sony[[i]] |
      'Oklahoma' %in% sony[[i]] |
      'Tennessee' %in% sony[[i]] |
      'South Dakota' %in% sony[[i]] |
      'Indiana' %in% sony[[i]] |
      'Maine' %in% sony[[i]] |
      'Colorado' %in% sony[[i]] |
      'Massachusetts' %in% sony[[i]] |
      'Connecticut' %in% sony[[i]] |
      'Kentucky' %in% sony[[i]] |
      'Arkansas' %in% sony[[i]] |
      'Alaska' %in% sony[[i]] |
      'Vermont' %in% sony[[i]] |
      'Iowa' %in% sony[[i]] |
      'Nevada' %in% sony[[i]] |
      'Montana' %in% sony[[i]] |
      'Maryland' %in% sony[[i]] |
      'Virginia' %in% sony[[i]] |
      'Missouri' %in% sony[[i]] |
      'New York' %in% sony[[i]] |
      'Texas' %in% sony[[i]] |
      'North Carolina' %in% sony[[i]] |
      'Alabama' %in% sony[[i]])){
      newfail <- data.frame(
        label = i
      )
      fail <- rbind(fail,newfail)
    }
  }else if(bundle == 'Primary'){
    if(name == 'Language'){
      if(!('English' %in% sony[[i]] |
        'Spanish' %in% sony[[i]] |
        'Other' %in% sony[[i]])){
        newfail <- data.frame(
          label = i
        )
        fail <- rbind(fail,newfail)
      }
    }else{
      if(!("True" %in% sony[[i]] | "False" %in% sony[[i]])){
        newfail <- data.frame(
          label = i
        )
        fail <- rbind(fail,newfail)
      }
    }
  }else if(bundle == 'Hours'){
    if(!(class(sony[[i]]) == 'numeric' | class(sony[[i]]) == 'integer')){
      newfail <- data.frame(
        label = i
      )
      fail <- rbind(fail,newfail)
    }
  }else if(bundle == 'Spend'){

  }else{
    newbundleRow <- data.frame(
      New_Labels = i
    )

    newbundle <- rbind(newbundle,newbundleRow)
  }
}

write.csv(newbundle, file = "~/Desktop/CSV/newbundle.csv")
write.csv(fail, file = "~/Desktop/CSV/incorrectbundle.csv")
