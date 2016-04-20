is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

load_or_install<-function(package_names)
{
  for(package_name in package_names)
  {
    if(!is_installed(package_name))
    {
      install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")
    }
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
}

dtm_idf<-function(dataf)
{
 
  reuters=Corpus(DataframeSource(data.frame(as.character(dataf)),encoding = "UTF-8"),readerControl = list(language = "en"))
  reuters <- tm_map(reuters, tolower)
  ## Remove Punctuations
  reuters <- tm_map(reuters, removePunctuation)
  ## Remove Numbers
  reuters <- tm_map(reuters, removeNumbers)
  reuters<-tm_map(reuters,stemDocument)
  ## Remove Stopwords
  reuters <- tm_map(reuters, removeWords, c("\\f","delivery","earliervery","every","everybody","everyone","everyones","everything","thinvery","i","me","everyth","my","myself","we","our","ours","ourselves","you","your","yours","yourself","yourselves","he","him","his","himself","she","her","hers","herself","it","its","itself","they","them","their","theirs","themselves","what","which","who","whom","this","that","these","those","am","is","are","was","were","be","been","being","have","has","had","having","do","does","did","doing","would","should","could","ought","i'm","you're","he's","she's","it's","we're","they're","i've","you've","we've","they've","i'd","you'd","he'd","she'd","we'd","they'd","i'll","you'll","he'll","she'll","we'll","they'll","let's","that's","who's","what's","here's","there's","when's","where's","why's","how's","a","an","the","and","but","if","or","because","as","until","while","of","at","by","for","with","about","against","between","into","through","during","before","after","above","below","to","from","up","down","in","out","on","off","over","under","again","further","then","once","here","there","when","where","why","how","all","any","both","each","few","other","some","such","only","own","same","so","than","call","also","inform","yes","will","know","date","said","can","want","take","taken","worked","global","sep","work","duration","years","till","countires","using","bank","highlisghts","etc","if","summary","new","india","indian","share","involved","details","used","university","yrs","the","about","after","all","also","an","and","another","any","are","as","at","be","because","been","before","being","between","both","but","by","came","can","come","could","did","do","each","for","from","get","got","has","had","he","have","her","here","him","himself","his","how","if","in","into","is","it","make","many","me","might","more","my","now","of","on","only","or","other","our","said","same","see","should","since","some","still","such","take","than","that","the","their","them","then","there","these","they","this","those","through","to","too","under","up","was","way","we","were","what","where","which","while","who","with","would","you","your","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","$","_","aaadd","aaannnnd","aab","aae","aashnas","aaye","abh","able","aboslutely","absolute","absolutelly","absolutely","absolutley","absolutly","aeade","aeddcbd","affb","afl","africa","afternoon","agfg","agin","across","actual","actually","adelaide","adelaides","adults","advacado","adityas","aiden","deliveryfood","deliveryman","everybodi","everyday","everyon","aidens","ahluwalia","aioli","airi","aisa","ajneet","aka","alla","alone","aloo","aman","ambe","amigospizzeriacomau","amit","amritsari","anand","andenjoy","andhra","angeles","anitas","ankita","anne","annw","annya","anzac","arabian","archivezwiftcomau","arcticlike","array","arun","ashfield","ashmits","ashtahmi","asia","asian","ask","asked","asking","auburn","aufsucuu","augul","australia","australian","australianchinese","australias",  "avnas","awadhi","baci","bacon","badrinath","bafat","bailan","bainjee","balsamic","banaba","banana","banarsi","bangude","bappa","baramundi","barbara","bathurst","bay","berado",
                                            "berardo","biryani","biriyani","build","bum","bbf","beaumaris","beejamanoli","ben","brahmins","barbecue","bars","barwant","baseball","beacon","beach","bedp","beef","beebeupkari","belgium","belly",
                                            "beec","beer","beers","bean","beans","bear","bearys","boston","bhaanvey","bird","bhaiya","boyssahil","bread","briyani","broccoli","bronwyn","brownie","buckman",
                                            "breads","bhaji","boothai","bhatti","bhia","burger","burgers","bhola",
                                            "bhole","business","cabanossi","cabramattaabout","calamari","caesar","businesses","called","calling","calls","calzone","camellia","cameronsmith",
                                            "candles","caramel","cardamom","career","cateringamigospizzeria","catergory",
                                            "catering","cctv","central","chetans","chettinad",
                                            "cdb","chamith","chaturthi","cheesecake","chickenwe","choori",
                                            "cdfe","cema","cnn","coastal","coated","coconut","cannoli","chops","cans","choy","chrismas","caribbean",
                                            "carly","caroline","carrot","cilantroshallot","chutti","case","chutney",
                                            "chutneys","clear",
                                            "cashew","caviar","clipsal",
                                            "cloes",
                                            "cashews","college",
                                            "casual","catholic","contact","curteous",
                                            "contactgingerindiancomau","cookbook","copper","cricket",
                                            "catholics","culturebring","daal",
                                            "daali","dal","desai","devilss",
                                            "dalal","delhi","delish","dhinkchika",
                                            "diane",
                                            "cathryn","celebratingindranis",
                                            "celebratingrabindra","chaat",
                                            "chacha","commentpicturesvideo",
                                            "chache","cucumber",
                                            "chain","desi",
                                            "chakko","champagnesoaked",
                                            "chana","chef",
                                            "chefs",
                                            "cherie","chimney",
                                            "chinatown","colleagues","padegam",
                                            "chinese","darshan","disappointedcheck","ditti","optuscomauunsub",
                                            "database",
                                            "daughter",
                                            "day",
                                            "days",
                                            "dca","pachranga",
                                            "dcb","drumsticks",
                                            "dcdb",
                                            "ddd",
                                            "dddc","hawaiian","haywards",
                                            "chineseasian","doughit","eduaubdedubu","hassle",
                                            "chip","function",
                                            "chips","friendlyhelpful","four","harpreets",
                                            "communities","glockl","gnocchi","guildford","haveli",
                                            "community","galato","gardenoutdoor","gate","gingerwhenever",
                                            "gave","google","gourmetrecipe","hai",
                                            "gavranic","gets","gem","giuliano","girlfriend","hip",
                                            "communitys","grandmothers","haidnkg","hican",
                                            "chipsonion","gokul","haryali","harvinder",
                                            "chocaron","greystanes","grier","halloween","hey",
                                            "chocaroons","goa","grkeep","grubfinder",
                                            "gruyere","gssuuuuu","guestswere",
                                            "goan","haiyan","hamburger",
                                            "hamburgers","hazelnut","hell","hollandaise",
                                            "holroyd","homework",
                                            "hona",
                                            "holy",
                                            "home",
                                            "hello",
                                            "helloooo","hes",
                                            "goat",
                                            "gobhi","gol","hear",
                                            "heard",
                                            "golab",
                                            "gold",
                                            "gobi",
                                            "god",
                                            "chocchip",
                                            "chock",
                                            "chocolate",
                                            "christine",
                                            "christmas",
                                            "chuda",
                                            "class","click",
                                            "clients",
                                            "classes","climate","coffee",
                                            "coffees",
                                            "cognac",
                                            "coke",
                                            "climbing",
                                            "domenico",
                                            "dominos","dosa",
                                            "dosancurry",
                                            "dosas",
                                            "dpubt",
                                            "dragon",
                                            "drama",
                                            "dussera",
                                            "dusshara",
                                            "dustbin",
                                            "dusted",
                                            "dustedwish",
                                            "httpaolitqzuoe",
                                            "httpbitlyahryvj",
                                            "httpbitlycts",
                                            "httpowlyrlvx","langi","myriad",
                                            "httpowlyrlwu","kfc","mrcc","mughals","mushroom",
                                            "music","mutton","nicepizzas","newington",
                                            "muslims","naan","nadu","ndtvcookscom","north",
                                            "northmead",
                                            "norwood",
                                            "notch",
                                            "nahin","nippies","nightkailash",
                                            "mughlai","nov",
                                            "november",
                                            "nsw",
                                            "nuggets",
                                            "number",
                                            "numerous",
                                            "nums",
                                            "nut",
                                            "nyc",
                                            "nysas",
                                            "oatlands",
                                            "muglai","neeraj",
                                            "httpowlyrlzlt","maranda","mns",
                                            "httpowlyrmks","masterchef",
                                            "httpowlyrobkl","kher","kildaprahran","kians",
                                            "khoury","kimchiite",
                                            "httpowlyrocgm","hum","kofta","mars","mmmmmm",
                                            "mas","march","mig\\EC\\89\\E2\\E2\\E5","mongolian",
                                            "monk",
                                            "konkani","lacha","makhani","moong",
                                            "moreya",
                                            "morning",
                                            "makhni","malhotra","malai",
                                            "malaysian","mon",
                                            "monday",
                                            "mondays",
                                            "male","mangalore",
                                            "mangalorean",
                                            "mango",
                                            "mangoes",
                                            "minuteshighly","mount",
                                            "mountain",
                                            "mountains",
                                            "mousse",
                                            "mall",
                                            "mam",
                                            "man",
                                            "malkit",
                                            "korai",
                                            "korea",
                                            "kori","kudi",
                                            "kueh",
                                            "kulcha",
                                            "kulfi",
                                            "kung",
                                            "kya",
                                            "laal",
                                            "korma",
                                            "humans",
                                            "paige",
                                            "pain",
                                            "painted",
                                            "pair",
                                            "pairing",
                                            "pakistan",
                                            "pakora",
                                            "pakoras",
                                            "palak",
                                            "pamela",
                                            "paneer",
                                            "panfried",
                                            "pani",
                                            "panipat",
                                            "panna",
                                            "panner",
                                            "papadums","pcs",
                                            "papagiannis","priyanka","sims","succulent",
                                            "papdi","rasam","sachin",
                                            "rashes","puree","ppl","rydalmere","saffron","sixth",
                                            "pradeep","queenslandthis","raw","seeds","slasa","snehitas",
                                            "snitzel","son","specialginger","strawberries","sullivan",
                                            "song","soldier",
                                            "salsa","scallopini","seems","smooshedfolded",
                                            "seen","semi","sevencity",
                                            "semifinals","september","shaam","shobhitvjyahoode",
                                            "shahi","spaghetti","spiral",
                                            "spanish",
                                            "shake","sms","spinnoccoli",
                                            "shakkar","singh",
                                            "singhs",
                                            "ser",
                                            "series","setia","sinha",
                                            "sipahee",
                                            "sir","sophies",
                                            "sorpotel",
                                            "sirloins",
                                            "sirmadam",
                                            "sit",
                                            "site",
                                            "sits",
                                            "sitting",
                                            "send","sullivan",
                                            "senior",
                                            "razala","road","rogan",
                                            "salami","salad","salivation",
                                            "salmon","samosa",
                                            "samosas","san","santa","sanoo","saturdayand",
                                            "saturdayomg","seafood",
                                            "seamers",
                                            "season",
                                            "seasonal","szechuan",
                                            "seasons","thenvincents",
                                            "santaa","wheels","wonderfulwhats",
                                            "santaas","uauuuuu","vandana","vegetable","yadav",
                                            "vegetables","yearthe","youd",
                                            "vegetarian","vicharaa","zwift",
                                            "vandanahwz","watermelon","wss","zamidara",
                                            "zamindaar","yianni",
                                            "wwwtamarintandooricomau","youyour",
                                            "younan",
                                            "xmas","yiros",
                                            "xoxo",
                                            "xxx",
                                            "weekenderbefore","wowexcellent",
                                            "waz","woolworths",
                                            "vandy","verry",
                                            "verses",
                                            "version",
                                            "vanessa",
                                            "ubu","usualnothing",
                                            "ucau",
                                            "ucbucaucbuccd",
                                            "ucsucuuu",
                                            "usuc",
                                            "uttar",
                                            "uuauf",
                                            "uudu","wigram",
                                            "wil",
                                            "william",
                                            "williams",
                                            "uudubu",
                                            "uuuauuf","walked",
                                            "walking",
                                            "walla",
                                            "wallet",
                                            "walls",
                                            "walnut",
                                            "wamr",
                                            "uuuc",
                                            "uuudsluu",
                                            "vada",
                                            "ucucau",
                                            "ucuccucauccducb",
                                            "ucueuauduaue",
                                            "ucuufufu",
                                            "udupi",
                                            "ueubuufue",
                                            "ulubuufucl",
                                            "uluuu",
                                            "sap",
                                            "saraswat",
                                            "sarita",
                                            "sasam",
                                            "sat",
                                            "satay",
                                            "sate",
                                            "sandeep",
                                            "salads",
                                            "rohans",
                                            "pradesh",
                                            "puri",
                                            "rasmalai","pudhina","raita",
                                            "rajah",
                                            "rajasthan",
                                            "rajasthani",
                                            "puli","rosary",
                                            "rose",
                                            "rosehill",
                                            "rosemary",
                                            "roses",
                                            "roseville",
                                            "rosewater",
                                            "rotary",
                                            "rotti",
                                            "rasmalaiiiiiiii",
                                            "raspberry",
                                            "paper","persia",
                                            "persian",
                                            "person",
                                            "pappadums",
                                            "parammatta",
                                            "paranthas",
                                            "parcels",
                                            "parents",
                                            "park",
                                            "parking",
                                            "parklets",
                                            "parmesan",
                                            "parmi",
                                            "parmigiana",
                                            "parmigianas",
                                            "parmy",
                                            "parramasala",
                                            "parramasalahope",
                                            "parramatta","pasta",
                                            "pastas",
                                            "pastaspeedy",
                                            "paste",
                                            "pastry",
                                            "parsley",
                                            "patricks",
                                            "patrode",
                                            "patronage",
                                            "patrons",
                                            "patta",
                                            "patty",
                                            "paul","pav"))
  ## Eliminating Extra White Spaces
  reuters <- tm_map(reuters, stripWhitespace)
#   BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#   dtm <- DocumentTermMatrix(reuters, control = list(tokenize = BigramTokenizer))
#   ## create a term document matrix
  dtm <- DocumentTermMatrix(reuters)
  dtm_tfxidf <- suppressWarnings(weightTfIdf(dtm))
  dtm1 <- as.matrix(dtm_tfxidf)
  #dtm1 <- as.matrix(dtm)
  dtm1=dtm1[,nchar(colnames(dtm1))<15]
  datfdtm=as.data.frame(dtm1)
  return(datfdtm)
}

  rm(list = setdiff(ls(), lsf.str())) 
  setwd("D:/R/SocialMedia/Naive Byess")
  load_or_install(c("Rcpp","Rook","plyr","RWeka","e1071","stringr","mlbench","tm","wordcloud","Rstem","tm.plugin.tags"))
  datae=read.csv("eatibility.csv",header=T,stringsAsFactors=FALSE)[c("review","sentimentScore")]
  dataen=read.csv("eatnow.csv",header=T,stringsAsFactors=FALSE)[c("review","sent_score")]
  datamn=read.csv("menulog.csv",header=T,stringsAsFactors=FALSE)[c("review","Sentiment")]
  datat=read.csv("truelocal.csv",header=T,stringsAsFactors=FALSE)[c("review","sent_score")]
  datafc=read.csv("fbcomments.csv",header=T,stringsAsFactors=FALSE)[c("message","sent_score")]
  #datafp=read.csv("fbposts.csv",header=T,stringsAsFactors=FALSE)[c("message","sent_score")]
  colnames(datae)[1:2]=c("review","sscore")
  colnames(dataen)[1:2]=c("review","sscore")
  colnames(datamn)[1:2]=c("review","sscore")
  colnames(datat)[1:2]=c("review","sscore")
  colnames(datafc)[1:2]=c("review","sscore")
  #colnames(datafp)[1:2]=c("review","sscore")
  #dataf=data.frame(rbind(datae,dataen,datamn,datat,datafc,datafp),stringsAsFactors=FALSE)
  dataf=data.frame(rbind(datae,dataen,datamn,datat,datafc),stringsAsFactors=FALSE)
  dataf[is.na(dataf[,1]),1]="NONE"
  
  datfdtm=dtm_idf(dataf[,1])
  cat=c(1:nrow(dataf))
  cat[dataf[,2]<=5&dataf[,2]>0]="good"
  cat[dataf[,2]>=-5&dataf[,2]<0]="bad"
  cat[dataf[,2]>5]="very good"
  cat[dataf[,2]< -5]="very bad"
  cat[dataf[,2]==0]="NONE"
#   cat[dataf[,2]<=5&dataf[,2]>0]=15
#   cat[dataf[,2]>=-5&dataf[,2]<0]=8
#   cat[dataf[,2]>5]=25
#   cat[dataf[,2]< -5]=3
#   cat[dataf[,2]==0]=0
  dat=cbind(as.factor(cat),datfdtm)
  colnames(dat)[1]="Class"
#   index <- sample(nrow(dat), 0.7 * nrow(dat))
#   train <- dat[index, ]
#   test <- dat[-index,]
  #train[,2]=as.numeric(train[,2])
  #model <- naiveBayes(train[,2] ~ train[,1], data=train, class.prob=TRUE)
  model <- naiveBayes(Class ~ ., data=dat, class.prob=TRUE)
  rm(list = setdiff(ls(), c(lsf.str(),"model")))
  save.image(file = "Model.RData")
  