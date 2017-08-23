
Share1 <-Expecteds[85-80+MaxMOV]
Share2 <- 1-Share1
Team1 <- grep("Rochester_NY", teams$Team)
Team2 <- grep("Marietta", teams$Team)

cat(c(Team1, Team2))

A[ Team1 ,Team2 ]=A[ Team1 ,Team2  ]+Share2;
A[ Team2 ,Team1 ]=A[ Team2 ,Team1  ]+Share1;
A[ Team1 ,Team1 ]=A[ Team1 ,Team1  ]+Share1;
A[ Team2 ,Team2 ]=A[ Team2 ,Team2  ]+Share2;

print(image(log(A)))
