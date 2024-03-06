//Javascript code for guessing game
//Work by Franziska Brändle

var clickEventType = "click";
var hoverEventType = "mouseover";
var unhoverEventType = "mouseout";


//Experiment Parameters
var fullurl = document.location.href, //url of incoming MTurk worker
  countdown = 60*10, //60 * 10,
  minMean = 20,
  maxMean = 80,
  possibleVariances = [0.1,1,10,100,1000]


  gameCounter = 0,
  roundsPerGame = 10,
  roundCounter = 0,
  playedRounds = [],
  currentVariance = 0,
  variancePerGame = [],
  currentMean = 0,
  meanPerGame = [],
  currentValue = 0,
  valuePerRound = [],
  valuePerGame = [],
  currentGuess = 0,
  guessPerRound = [],
  guessPerGame = [],
  currentDifference = 0,
  differencePerRound = [],
  differencePerGame = [],
  currentScore = 0,
  scorePerRound = [],
  scorePerGame = [],
  totalScore = 0,
  currentFun = 0,
  funOfGame = 0,
  gameFun = [],
  currentAverage = 0,
  averagePerGame = [],
  machines = [],

  timeGuessPerRound = [],
  timeGuessPerGame = [],
  timeNextGuessPerRound = [],
  timeNextGuessPerGame = [],
  timeEndGame = [],
  timeFunPerRound = [],
  timeGameFun = [],

  guessTouched = false,
  funTouched = false,
  gameFunTouched = false,

  ageTouched = false,
  age = 0,
  genderAnswered = false,
  gender = "",
  remarks="",
  machinenames = ["&#x2C00", "&#x2C01", "&#x2C02","&#x2C03", "&#x2C04", "&#x2C05","&#x2C06", "&#x2C07", "&#x2C08","&#x2C09","&#x2C0A","&#x2C0B","&#x2C0C","&#x2C0D","&#x2C0E","&#x2C0F","&#x2C10", "&#x2C11", "&#x2C12","&#x2C13", "&#x2C14", "&#x2C15","&#x2C16", "&#x2C17", "&#x2C18","&#x2C19","&#x2C1A","&#x2C1B","&#x2C1C","&#x2C1D","&#x2C1E","&#x2C1F","&#x2C20", "&#x2C21", "&#x2C22","&#x2C23", "&#x2C24", "&#x2C25","&#x2C26", "&#x2C27", "&#x2C28","&#x2C29","&#x2C2A","&#x2C2B","&#x2C2C","&#x2C2D","&#x2C2E"]
  currentExplanation = 0,
  instructionsCounter = 0;

var imageStr = '';
for(i=0; i<10; i++){
  imageStr += '<img src="Instructions/'+i+ '.PNG">';
}
document.getElementById('imageloader').innerHTML = imageStr;

function assignGame() {
  gameCounter += 1;
  roundCounter = 0;
  valuePerRound = [];
  guessPerRound = [];
  differencePerRound = [];
  scorePerRound = [];
  timeGuessPerRound = [];
  timeFunPerRound = [];

  //create a new normal distribution for the game
  currentVariance = possibleVariances[Math.floor(Math.random()*possibleVariances.length)];
  variancePerGame.push(currentVariance);
  currentMean = randomNum(minMean,maxMean);
  meanPerGame.push(currentMean);

  //reset guessing page
  guessTouched = false;
  document.getElementById("guessSlider").value = 50;
  guessOutput.innerHTML = "Please move the slider";
  document.getElementById("buttonSubmitGuess").style.visibility = "visible";
  document.getElementById("buttonEndGame").style.visibility = "hidden";
  document.getElementById("guessSliderArea").style.visibility = "visible";
  document.getElementById("buttonNextGuess").style.visibility = "hidden";
  change("finalGuessValue", "<font size='5'><p></p></font>");
  change("solutionValue", "<font size='5'><p></p></font>");
  change("differenceValue", "<font size='5'><p></p></font>");

  //set new machine symbol
  currentmachine = machinenames[Math.floor(Math.random()*machinenames.length)];
  if(machines.length<=machinenames.length){ //workaround just to make sure that if people use all machines, the experiment still runs. I don't think it will happen.
    while (machines.includes(currentmachine)){
      currentmachine = machinenames[Math.floor(Math.random()*machinenames.length)];
    }
  }
  change("machinetext", "<p>" + currentmachine + "</p>");
  machines.push(currentmachine);

  //change to guessing page
  clickStart("startGame","backgroundmachine");
}


function evaluateGuess() {

  if(guessTouched == false)
  {
    //alert("You need to interact with the slider before moving on.");
    document.getElementById("NoGuessModal").style.display = "block";
  }
  else{

    document.getElementById("buttonSubmitGuess").style.visibility = "hidden";
    document.getElementById("guessSliderArea").style.visibility = "hidden";
    document.getElementById("machineWorkingImage").style.visibility = "visible";

    //calculate current data
    var d = new Date();
    timeGuessPerRound.push(d.getTime());
    roundCounter += 1;
    currentGuess = +(document.getElementById("guessSlider").value);
    guessPerRound.push(currentGuess);
    currentValue = Math.round(myNorm(currentMean, currentVariance));
    //check if value is in range, if not resample
    while(currentValue > 100 || currentValue < 1){
      currentValue = Math.round(myNorm(currentMean, currentVariance));
    }
    valuePerRound.push(currentValue);
    currentDifference = Math.abs(currentValue-currentGuess);
    differencePerRound.push(currentDifference);

    currentScore = 10 - currentDifference;
    if (currentScore < 0){
      currentScore = 0;
    }
    scorePerRound.push(currentScore);
    totalScore += currentScore;


    //update guessing page
    change("finalGuessValue", "<p>"+currentGuess+"</p>");

    setTimeout(function(){

      change("solutionValue", "<p>"+currentValue+"</p>");
      setTimeout(function(){
        change("differenceValue", "<p>"+currentDifference+"</p>");
        document.getElementById("buttonSubmitGuess").style.visibility = "hidden"
        document.getElementById("buttonNextGuess").style.visibility = "visible";
        document.getElementById("machineWorkingImage").style.visibility = "hidden";

        if(roundCounter >2){
          document.getElementById("buttonEndGame").style.visibility = "visible";
        }
      },750)

    }, 750);
  }
}

function nextGuess(){
  if(timeEnded == true){
    endGame();
  }
  else{
  //reset sliders
    var d = new Date();
    timeNextGuessPerRound.push(d.getTime());
    document.getElementById("guessSlider").value = 50;
    guessOutput.innerHTML = "Please move the slider";

    document.getElementById("buttonSubmitGuess").style.visibility = "visible";
    document.getElementById("buttonEndGame").style.visibility = "hidden";
    document.getElementById("guessSliderArea").style.visibility = "visible";
    document.getElementById("buttonNextGuess").style.visibility = "hidden";

    change("finalGuessValue", "<font size='5'><p></p></font>");
    change("solutionValue", "<font size='5'><p></p></font>");
    change("differenceValue", "<font size='5'><p></p></font>");

    guessTouched = false;

    //console.log(roundCounter)

    document.getElementById("buttonSubmitGuess").style.visibility = "visible";

  }
}

function submitGameFun(){
  if(gameFunTouched == false)
  {
    //alert("You need to interact with the slider before moving on.");
    document.getElementById("NoGuessModal").style.display = "block";
  }
  else{

    var d = new Date();
    timeGameFun.push(d.getTime());

    funOfGame = document.getElementById("gameFunSlider").value;
    gameFun.push(funOfGame);

    if (timeEnded == true){
      clickStart("page3","page4a")
    }
    else{

      clickStart("rateGame", "startGame")
    }
  }
}



function endGame(){
  var d = new Date();
    timeEndGame.push(d.getTime());
  valuePerGame.push(valuePerRound);
  guessPerGame.push(guessPerRound);
  differencePerGame.push(differencePerRound);
  scorePerGame.push(scorePerRound);
  timeGuessPerGame.push(timeGuessPerRound);
  timeNextGuessPerGame.push(timeNextGuessPerRound);
  playedRounds.push(roundCounter);

  currentAverage = Math.round(scorePerRound.reduce((a, b) => a + b, 0)/roundCounter);
  averagePerGame.push(currentAverage);


  document.getElementById("gameFunSlider").value = 4;
  gameFunOutput.innerHTML = "Please move the slider";
  gameFunTouched = false;
  clickStart("backgroundmachine","rateGame")

}

function goToQuestionnaire(){
  ageOutput.innerHTML = "Please move the slider";
  clickStart("page4b","page5")
}

function goToThankYou(){
  clickStart("page4a","page4b")
}

function endExperiment(){
  if(document.getElementById('g1').checked || document.getElementById('g2').checked || document.getElementById('g3').checked || document.getElementById('g4').checked){
    genderAnswered = true;
  }

  if (ageTouched == true && genderAnswered == true){
    if(document.getElementById('g1').checked){
      gender="female";
    }
    if(document.getElementById('g2').checked){
      gender="male";
    }
    if(document.getElementById('g3').checked){
      gender="diverse";
    }
    if(document.getElementById('g4').checked){
      gender="no answer";
    }

    remarks = document.getElementById('freeAnswerRemarks').value;
    goals = document.getElementById('freeAnswerGoal').value;
    age = document.getElementById('ageSlider').value;

    senddata()
    change("surveytext", "<p><b> bumblebee </b></p>");
    clickStart("page5","end")
  }
  else{
    //alert("You need to answer the questions before moving on.");
    document.getElementById("NotAllQuestionsModal").style.display = "block";
  }
}

function senddata() {

  //All data to save
  saveDataArray = {
    'variancePerGame': variancePerGame,
    'meanPerGame': meanPerGame,
    'playedRounds':playedRounds,
    'valuePerGame': valuePerGame,
    'guessPerGame': guessPerGame,
    'differencePerGame': differencePerGame,
    'scorePerGame': scorePerGame,
    'totalScore': totalScore,
    'gameFun': gameFun,
    'timeGuessPerGame': timeGuessPerGame,
    'timeNextGuessPerGame':timeNextGuessPerGame,
    'timeEndGame':timeEndGame,
    'timeGameFun':timeGameFun,
    'machines':machines,
    'age': age,
    'gender': gender,
    'goals': goals,
    'remarks': remarks,
    'instructionsCounter':instructionsCounter,
  };

  saveData(JSON.stringify(saveDataArray))
}

// extract URL parameters
function turkGetParam(name) {
  var regexS = "[\?&]" + name + "=([^&#]*)";
  var regex = new RegExp(regexS);
  if (typeof fullurl == "undefined"){
    console.log("fullurl, who?")
    return Math.floor(Math.random() * 10000);
  } else {
     var tmpURL = fullurl;
     var results = regex.exec(tmpURL);
     if (results == null) {
         return Math.floor(Math.random() * 10000);
     } else {
         return results[1];
  }
 }
}

var subjectID = turkGetParam('workerId');

function saveData(filedata){
  var filename = "./data/" + subjectID + "data_1.txt";
  $.post("results_data.php", {postresult: filedata + "\n", postfile: filename })
  console.log(filedata)
}

//update slider values
  var guessInput = document.getElementById("guessSlider");
  var guessOutput = document.getElementById("guessValue");
  guessOutput.innerHTML = guessInput.value;

  var gameFunInput = document.getElementById("gameFunSlider");
  var gameFunOutput = document.getElementById("gameFunValue");
  gameFunOutput.innerHTML = gameFunInput.value;

  var ageInput = document.getElementById("ageSlider");
  var ageOutput = document.getElementById("ageValue");
  ageOutput.innerHTML = ageInput.value;

  guessInput.oninput = function() {
    guessOutput.innerHTML = this.value;
    guessTouched = true;
  }

 ageInput.oninput = function() {
    ageOutput.innerHTML = this.value;
    ageTouched = true;
  }

  gameFunInput.oninput = function() {
    gameFunOutput.innerHTML = this.value;
    gameFunTouched = true;
  }


 //changes from one page to another
function clickStart(hide, show) {
  document.getElementById(hide).style.display = "none";
  document.getElementById(show).style.display = "block";
  window.scrollTo(0, 0);
}

//changes inner HTML of div with ID=x to y
function change (x,y){
    document.getElementById(x).innerHTML=y;
}

function submitConsentForm(){
  clickStart("page1a", "page1b")

}

function submitDataForm(){
  clickStart("page1b", "page2")

}

function nextExplanation(){
  if(currentExplanation == 9){ //9
    clickStart("page2", "page2a")
  }

  else{
    currentExplanation += 1;
    change("instructionPicture", '<center><img src="Instructions/'+ currentExplanation + '.PNG" alt="Instructions" width="80%"></center>');
    document.getElementById("buttonExplanationBack").style.visibility = "visible";

  }
}

function lastExplanation(){
  if(currentExplanation == 1){
    document.getElementById("buttonExplanationBack").style.visibility = "hidden";
  }


  currentExplanation -= 1;
  change("instructionPicture", '<center><img src="Instructions/'+ currentExplanation + '.PNG" alt="Instructions" width="80%"></center>');


}

function submitExplanation(){

  if (document.getElementById('q1c').checked) {
    var ch1 = 1
  }
  if (document.getElementById('q2b').checked) {
    var ch2 = 1
  }
  if (document.getElementById('q3c').checked) {
    var ch3 = 1
  }

  if (document.getElementById('q4b').checked) {
    var ch4 = 1
  }

  //are all of the correct
  var checksum = ch1 + ch2 + ch3 + ch4;

  if (checksum === 4) {
    display = document.querySelector('#time');
    startTimer(countdown, display);
    clickStart("page2a", "page3")
  }
  else{
    //if one or more answers are wrong, raise alert box
    //alert('You have answered some of the questions wrong. Please try again.');
    document.getElementById("WrongAnswerModal").style.display = "block";

    //go back to instructions
    instructionsCounter += 1;
    currentExplanation = -1;
    nextExplanation();
    clickStart("page2a", "page2");
    document.getElementById("buttonExplanationBack").style.visibility = "hidden";

  }
}

 //random number generator
function randomNum(min, max) {
  return Math.floor(Math.random() * (max - min) + min)
}

//Create normal distribution
function myNorm(mean, variance) {
  var x1, x2, rad, c;
  var stddev = Math.sqrt(variance)
  do {
    x1 = 2 * Math.random() - 1;
    x2 = 2 * Math.random() - 1;
    rad = x1 * x1 + x2 * x2;
  } while (rad >= 1 || rad == 0);
  c = Math.sqrt(-2 * Math.log(rad) / rad);
  return (x1 * c) * stddev + mean;
};

function saveText(text, filename){
  var a = document.createElement('a');
  a.setAttribute('href', 'data:text/plain;charset=utf-u,'+encodeURIComponent(text));
  a.setAttribute('download', filename);
  a.click()
}

var timeEnded = false;

function startTimer(duration, display) {
  console.log("Timer started");
  var timer = duration, minutes, seconds;
  var MyTimer = setInterval(function () {
    minutes = parseInt(timer / 60, 10);
    seconds = parseInt(timer % 60, 10);
    seconds = seconds < 10 ? "0" + seconds : seconds;

    if (--timer <= 0) {
      clearInterval(MyTimer);
      timeEnded = true;

    }
  }, 1000);
}



var span1 = document.getElementsByClassName("close1")[0];
var span2 = document.getElementsByClassName("close2")[0];
var span3 = document.getElementsByClassName("close3")[0];


window.onclick = function(event) {
  if (event.target == document.getElementById("NotAllAnswersModal")) {
    document.getElementById("NotAllAnswersModal").style.display = "none";
  }

  if (event.target == document.getElementById("WrongAnswerModal")) {
    document.getElementById("WrongAnswerModal").style.display = "none";
  }

  if (event.target == document.getElementById("NoGuessModal")) {
    document.getElementById("NoGuessModal").style.display = "none";
  }

}

span1.onclick = function() {
  document.getElementById("NoGuessModal").style.display = "none";
}

span2.onclick = function() {
  document.getElementById("NotAllAnswersModal").style.display = "none";
}

span3.onclick = function() {
  document.getElementById("WrongAnswerModal").style.display = "none";
}
