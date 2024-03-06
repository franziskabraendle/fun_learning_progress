//Javascript code for grid exploration game
//Original work Charley Wu, adapted by Franziska Brändle

// Touch events support
var clickEventType = "click";
var hoverEventType = "mouseover";
var unhoverEventType = "mouseout";

//EXPERIMENT PARAMETERS
var fullurl = document.location.href, //url of incoming MTurk worker
  totalRounds = 330, // 105 includes the instructions trial and the bonus trial
  rounds = totalRounds, //number of REMAINING rounds
  roundCounter = -1, //counter for current trial number
  horizon = 898, //898
  demoRound = true, // if the participant is still in the Instruction mode
  numberDemoRounds = 1, //how often the participant needs to read the instructions, defualt value 1
  countdown = 60 * 10,
  tracker = new Array(0), //tracker array
  investigationIndex = 0, //current click number
  scoretotal = [],
  scorecurrent = 0,
  reward = 0.00,
  starArray = [],
  gridMax = [],
  envOrder = [],
  currentEnv = envOrder[0],
  environmentList = [],
  scale = [],
  //Color parameters for heatmap
  colors = ['#fff7ec', '#fee8c8', '#fdd49e', '#fdbb84', '#fc8d59', '#ef6548', '#d7301f', '#b30000', '#7f0000'],
  heatmapColor = d3.scale.linear().domain(d3.range(0, 75, 75.0 / (colors.length - 1))).range(colors),
  testerNotes = {},
  valueRange = 40;

var orderedArray = [];
for(var i = 0; i <= 329; i++){
    orderedArray.push(i);
}
envOrder = getRandomSubarray(orderedArray, totalRounds);

//data collectors for search history
var tscollect = [],
  xcollect = [],
  ycollect = [],
  //zcollect = [],
  zcollectScaled = [],
  initcollect = [],
  bonusCollect = {"bonusCells":[], finalChosenCell:null};

for (var i = 0; i < totalRounds; i++) {
  scale[i] = randomNum(valueRange+5,75);
  scoretotal[i] = 0;
  starArray[i] = 0;
  gridMax[i] = 0;
  tscollect[i] = [];
  xcollect[i] = [];
  ycollect[i] = [];
  zcollectScaled[i] = [];
  initcollect[i] = [];
}

//Declare variables not yet assigned
var participantId,
  condition,
  gender,
  age = 0,
  birthDate,
  grade,
  searchHistory,
  initialEnvs,
  changeEnvs,
  roundScore,
  xout,
  yout,
  zout,
  optimaGuess;

var workerID=turkGetParam('workerId');

//Assign scenario
function assignScenario() {

    clicks = horizon; //set initial number of clicks to horizon
    initialEnvs = kernel12;
    //put environments from the randomized envOrder into environmentList
    for (i = 0; i <= rounds; i++) {
      environmentList[envOrder[i]] = initialEnvs[envOrder[i]];
    }
    //Advance the page
    nexttrial();
    clickStart('page1', 'page2');
}

//Checkers:
//initialize environments
var init = [];

function instructioncheck() {
  //check if correct answers are provided
  if (document.getElementById('q1b').checked) {
    var ch1 = 1
  }
  if (document.getElementById('q2d').checked) {
    var ch2 = 1
  }
  if (document.getElementById('q3d').checked) {
    var ch3 = 1
  }
  //are all of the correct
  var checksum = ch1 + ch2 + ch3;
  if (checksum === 3) {

	clickStart('page3', 'page4');
  }
  else {
    //if one or more answers are wrong, raise alert box
    alert('You have answered some of the questions wrong. Please try again.');
    //go back to instructions
    instructionsCounter = 1;
    document.getElementById("sidebarInstructions").style.display = "block";
    toggleNext = 0;

    clickStart('page3', 'page2');
    onButtonInstructionsPressed();
  }
}

function startExperiment(){
	//clicks = 0;
    gridDeactivated = false;
    demoRound = false;
    nexttrial();
    clickStart('page4', 'page2');

    display = document.querySelector('#time');
    startTimer(countdown, display);

}


/**
 * Sets the button handlers
 */
function setButtonHandlers() {
  document.getElementById("finishButton").addEventListener(clickEventType, function () {
    //senddata();
    onButtonFinishPressed();
  });
  document.getElementById("nextTrialButton").addEventListener(clickEventType, function () {
    nexttrial();
  });
  document.getElementById("buttonInstructionsCheck").addEventListener(clickEventType, function () {
    instructioncheck();
  });
  document.getElementById("buttonGoToPageThree").addEventListener(clickEventType, function () {
    clickStart('page2', 'page3');
  });

  document.getElementById("buttonInstructions").addEventListener(clickEventType, function () {
    onButtonInstructionsPressed();
  });
  document.getElementById("buttonNext").addEventListener(clickEventType, function () {
    chooseNextButtonFunction();
  });

}


/**
 * Creates the grid in the DOM
 */
function createGrid() {
  var i, j, WIDTH = 30,
    HEIGHT = 30,
    lastRevealedCellId;

  var table = document.createElement("table");
  table.setAttribute("id", "grid");
  table.setAttribute("class", "grid");

  for (var y = 0; y < HEIGHT; y++) {
    var tr = document.createElement("tr");
    for (x = 0; x < WIDTH; x++) {
      var td = document.createElement("td");
      //td.setAttribute("id", y + "x" + x);
      td.setAttribute("data-x", x);
      td.setAttribute("data-y", y);
      td.addEventListener(clickEventType, onCellTappedHandler);
      td.addEventListener(hoverEventType, hoverHandler);
      td.addEventListener(unhoverEventType, unhoverHandler);
      tr.appendChild(td);
    }
    table.appendChild(tr);

    var gridContainer = document.getElementById("gridDiv");
    gridContainer.innerHTML = "";
    gridContainer.appendChild(table);
  }
}



function hoverHandler(evt){

  	var x = evt.target.getAttribute("data-x");
  	var y = evt.target.getAttribute("data-y");
  	var cell = Cell.getCell(x, y);

  	if (!cell.hasHistory()){
		evt.target.setAttribute("style", "background-color:#e6e6e6;")
	}
}

function unhoverHandler(evt){
	var x = evt.target.getAttribute("data-x");
  	var y = evt.target.getAttribute("data-y");
  	var cell = Cell.getCell(x, y);

  	if (!cell.hasHistory()){
		evt.target.setAttribute("style", "background-color:white;")
	}
}


/**
 *  onCellTappedHandler()
 * @param {*} evt
 */
function onCellTappedHandler(evt) {

  // get tapped Cell instance
  var x = evt.target.getAttribute("data-x");
  var y = evt.target.getAttribute("data-y");
  var cell = Cell.getCell(x, y);

  return onCellTapped(cell);

}

// Instructions status
var instructionsCounter = 2;
var gridDeactivated = false;

/**
 *  button Instructions event handler
 * @param {*} evt
 */
function onButtonInstructionsPressed(evt) {

  if (instructionsCounter == 0) {
    // go to insturctions 2nd page, not used at the moment
    document.getElementById("sidebarInstructions-1").style.display = "none";
    document.getElementById("sidebarInstructions-3").style.display = "block";
    if (clicks > 0) gridDeactivated = false;
    instructionsCounter=2;
    goToNextGrid();
  }

  else if (instructionsCounter == 1) {

    document.getElementById("sidebarInstructions-1").style.display = "block";
    //document.getElementById("sidebarInstructions-3").style.display = "none";
    //goToNextGrid(); //activate to have a new grid every time the participant answers a question wrong.
    instructionsCounter=2;
  	numberDemoRounds = numberDemoRounds +1;

  }

  else if (instructionsCounter == 2) {
    clickStart('page2', 'page3');
    document.getElementById("sidebarInstructions").style.display = "none";
    //document.getElementById("sidebarInstructions-3").style.display = "none";
    //document.getElementById("sidebarInstructions-1").style.display = "block";
    toggleNext = 1;
  }
}

function onButtonFinishPressed() {

  var optionA = document.querySelector('input[name="option-a"]:checked');
  if (optionA == null) {
    return alert("Fill option A!");
  }

  var notes = document.getElementById("tester-notes").value;

  testerNotes = {
    "option-a": parseInt(optionA.value),
    "notes": notes
  };

  senddata();
}


/**
 *  onCellTapped()
 * @param {Cell} cell
 */
function onCellTapped(cell) {

  if (cell == null || gridDeactivated || cell.hasHistory() || rounds<0 || experimentEnded==true ) return;

    //check if there are more clicks available
      //if () {}
  if (clicks <= 0) {
    if (rounds == 0) {
      document.getElementById("nextTrialButton").onclick = clickStart('page5', 'page7');
        // Game over, calculate final score, not used at the moment
        GameOverScore = finalPerformance(scoretotal);
    }
    else{
      alert('Please click on "Next", because you have clicked on all possible cells.');
      return;
    }
  }
  else{


    cell.clicked();

    // update counters
    currentEnvNum = envOrder[roundCounter];
    investigationIndex = investigationIndex + 1;
    tracker[investigationIndex] = cell.x + "x" + cell.y;

    //update number of clicks left
    clicks = clicks - 1;
    //change("remaining2", "Remaining Clicks: <b>" + clicks + "</b>");

    //Update maximum reward found
    if (cell.rescaledValue > gridMax[roundCounter]) {
      gridMax[roundCounter] = cell.rescaledValue;
    }

    //keep track of tapped cell
    var d = new Date();
    tscollect[roundCounter][investigationIndex] = d.getTime();
    xcollect[roundCounter][investigationIndex] = cell.x;
    ycollect[roundCounter][investigationIndex] = cell.y;
    //zcollect[roundCounter][investigationIndex] = cell.noiseyValue;
    zcollectScaled[roundCounter][investigationIndex] = cell.rescaledValue;

    // update score
    scorecurrent = Math.round(cell.rescaledValue);
    scoretotal[roundCounter] = scoretotal[roundCounter] + scorecurrent;
    reward = rewardCum(scoretotal);
    roundScore = performanceScore(scoretotal[roundCounter], scale[roundCounter]);
    //change('scoretotal', "Points: " + scoretotal[roundCounter]);


  }
}



var experimentEnded = false;

function goToNextGrid() {
  if(timeEnded == false){
      nexttrial();
  }
  else{
  	experimentEnded = true;
  	clickStart('page2', 'page7');
  }
}



var toggleNext = 0;

function chooseNextButtonFunction(){
	if(clicks > (horizon-4)){
		alert("Please open at least five tiles before moving on.");
	}
	else{

		if (toggleNext == 0){
	  		onButtonInstructionsPressed();
		}
		else{
	  		goToNextGrid();
		}
	}
}

//not used at the moment
function BackButton(){
	instructionsCounter = 1;
	onButtonInstructionsPressed();
}




//code for questionnaire in the end
var questionnaireValue = [];
var numberQuestions1 = 2;
var numberQuestions2 = 4;
var textBoxValue = "";
var textBoxValue1 = "";
var textBoxValue2 = "";
var textBoxValue3 = "";
var textBoxValue4 = "";
var textBoxValue5 = "";

questionnaireValue.length = numberQuestions1 + numberQuestions2;
questionnaireValue.fill(0);

function submitQuestionnaire1(){
	var numberAnswers = 7;
	var allQuestionsAnswered = true;



	for (var currentQuestion = 1; currentQuestion<=numberQuestions1; currentQuestion++){

		var answerChecked = false;

		for(var j=0; j<numberAnswers; j++){
			currentElement = ("f"+currentQuestion+j)
			if(document.getElementById(currentElement).checked == true){
				answerChecked = true;
				questionnaireValue[(currentQuestion-1)] = document.getElementById(currentElement).value;
			}
		}

		if (answerChecked == false){
		allQuestionsAnswered = false;

		}

	}

	if (allQuestionsAnswered == false){
		alert("You need to answer every question!");
	}
	else{
		clickStart('page7', 'page8');
	}
}

function submitQuestionnaire2(){
	var numberAnswers = 7;
	var allQuestionsAnswered = true;


	for (var currentQuestion = numberQuestions1+1; currentQuestion<= (numberQuestions2 + numberQuestions1); currentQuestion++){

		var answerChecked = false;

		for(var j=0; j<numberAnswers; j++){

			currentElement = ("f"+currentQuestion+j)
			if(document.getElementById(currentElement).checked == true){
				answerChecked = true;
				questionnaireValue[(currentQuestion-1)] = document.getElementById(currentElement).value;
			}
		}


		if (answerChecked == false){
		allQuestionsAnswered = false;

		}

	}

	if (allQuestionsAnswered == false){
		alert("You need to answer every question!");
	}
	else{

		clickStart('page8', 'page9');
	}
}


function submitQuestionnaire3(){

	textBoxValue1 = document.getElementById("freeAnswer1").value;
	textBoxValue2 = document.getElementById("freeAnswer2").value;
	textBoxValue3 = document.getElementById("freeAnswer3").value;

	if (textBoxValue1.length < 1 || textBoxValue2.length < 1 || textBoxValue3.length < 1){
		alert("You need to answer every question!");
	}
	else{

		clickStart('page9', 'page10');
	}
}

function submitQuestionnaire4(){

	textBoxValue4 = document.getElementById("freeAnswer4").value;
	textBoxValue5 = document.getElementById("freeAnswer5").value;

	if (textBoxValue4.length < 1 || textBoxValue5.length < 1){
		alert("You need to answer every question!");
	}
	else{
		clickStart('page10', 'page6');
	}
}

////////////////////////////////////////
// Cell class
////////////////////////////////////////


/**
 * Cell Class
 *
 * @param {number} x
 * @param {number} y
 * @param {number} aValue
 * @param {number} nValue
 * @param {number} rValue
 */
function Cell(x, y, aValue, nValue, rValue) {

  /**
   * Cell properties
   */

  this.x = x;
  this.y = y;
  this.absoluteValue = aValue;
  this.noiseyValue = nValue;
  this.rescaledValue = rValue;
  this.history = [];


  /**
   * Cell methods
   */

  this.getTd = function () {
    var cells = document.querySelectorAll('td[data-x="' + this.x + '"][data-y="' + this.y + '"]');
    if (cells.length > 0) return cells[0];
    else return null;
  }

  this.addToHistory = function (value) {
    this.history.push(value);
    if (this.getTd()) this.getTd().setAttribute("title", this.history.toString());
  }

  this.hasHistory = function () {
    return this.history.length > 0;
  }

  this.setBgColor = function (color) {
    if (this.getTd()) this.getTd().style.backgroundColor = color;
  }

  this.clicked = function () {
    var td = this.getTd();
    if (this.hasHistory()) {
      //this.updateValue();
    } else {
      if (td) td.setAttribute("title", this.rescaledValue.toString());
      var bgcolor = heatmapColor(this.rescaledValue);
      this.setBgColor(bgcolor);

      this.addToHistory(this.rescaledValue);
      if (td) {
        td.innerHTML = this.rescaledValue;
        td.classList.toggle("highlight");
        setTimeout(function () {
          td.classList.toggle("highlight");
        }, 300);
      }
    }
  }

  this.setTempValue = function (value) {
    if (this.getTd()) this.getTd().innerHTML = value;
    var bgcolor = heatmapColor(value);
    this.setBgColor(bgcolor);
  }

  this.clearTempValue = function (value) {
    if (this.getTd()) this.getTd().innerHTML = "";
    this.setBgColor("transparent");
  }

  /**
   * Static properties and methods
   */

  Cell.updateEnvironment = function() {
    Cell.cells = [];
    for (i = 0; i < 30; i++) {
      Cell.cells[i] = [];
    }

    var env = environmentList[envOrder[roundCounter]];


    for (var k = 0; k <= 899; k++) {
      var x = env[k].x1;
      var y = env[k].x2;
      //var absoluteValue = env[k].y * 50;
      var absoluteValue = env[k].y;
      var noiseyValue = Math.round(absoluteValue + myNorm());
      var rescaledValue = Math.max(Math.round(absoluteValue * valueRange + (scale[roundCounter]-valueRange)), 0);
      //var rescaledValue = Math.max(Math.round(absoluteValue * scale[roundCounter] + 5), 0);
      //var rescaledValue = Math.max(Math.round(noiseyValue / 50 * scale[roundCounter] + 5), 0);
      var cell = new Cell(x, y, absoluteValue, noiseyValue, rescaledValue);
      Cell.cells[x][y] = cell;
      //cell.clicked(); //necessary for showing grids open
    }
  }

  Cell.getCell = function(x, y) {
    return Cell.cells[x][y];
  }

  Cell.getRandomCell = function() {
    var x = Math.floor(Math.random() * Cell.cells.length);
    var y = Math.floor(Math.random() * Cell.cells[x].length);
    return Cell.getCell(x, y);
  }

  Cell.getRandomCells = function(n) {
    randomCells = [];
    for (var i = 0; i < n; i++) {
      var found = false;
      while (!found) {
        var cell = this.getRandomCell();
        if (!cell.hasHistory() && randomCells.indexOf(cell) == -1) {
          randomCells.push(cell);
          found = true;
        }
      }
    }
    return randomCells;
  }


}
new Cell();







function nexttrial() {

  rounds = rounds - 1; // decrease remaining rounds

  if (rounds >= 0) {
    initcollect[roundCounter] = init; //retrieve initially revealed tile from previous trial before updating trial counter

    //update roundCounter
    roundCounter = roundCounter + 1;

    createGrid();
    Cell.updateEnvironment();


   	var firstCell = Cell.getRandomCell();
    firstCell.clicked();

    //store initial values
    var d = new Date();
    tscollect[roundCounter][0] = d.getTime();
    xcollect[roundCounter][0] = firstCell.x;
    ycollect[roundCounter][0] = firstCell.y;
    //zcollect[roundCounter][0] = firstCell.noiseyValue;
    zcollectScaled[roundCounter][0] = firstCell.rescaledValue; //store noisey value

    //update gridMax with initial tile
    gridMax[roundCounter] = firstCell.rescaledValue;
    scoretotal[roundCounter] = firstCell.rescaledValue;
    //Update text
    //change('scoretotal', "Points: " + scoretotal[roundCounter]);
    //go back to task
    clickStart('page5', 'page2');
    //renew investigations
    clicks = horizon;
    //renew investigationIndex
    investigationIndex = 0;
  }

  // If remaining rounds < 0 --> game ended
  if (rounds < 0) {
    //move to final page
    clickStart('page2', 'page7');
    alert('Thank you for participating. Please fill out the questionnaire.');
  }
}







function debugData() {
  console.log(tscollect);
  console.log(xcollect);
  console.log(ycollect);
  //console.log(zcollect);
}

function senddata() { //TODO: set up for tablet
  //search history
  searchHistory = {
    'tscollect': tscollect,
    'xcollect': xcollect,
    'ycollect': ycollect,
    'zcollectScaled': zcollectScaled
  };
  //All data to save
  saveDataArray = {
    'participantId': workerID, //set to workerID when putting it on MTurk
    'scale': scale,
    'envOrder': envOrder,
    'searchHistory': searchHistory,
    'questionnaireValue': questionnaireValue,
    'textBoxValue1': textBoxValue1,
    'textBoxValue2': textBoxValue2,
    'textBoxValue3': textBoxValue3,
    'textBoxValue4': textBoxValue4,
    'textBoxValue5': textBoxValue5,
    'numberDemoRounds': numberDemoRounds,

  };

  saveText(JSON.stringify(saveDataArray), 'gridSearch.gridData.'+participantId + '.JSON');
  clickStart('page6', 'rt-end');
}


//*************UTILITIES***************************************


//changes from one page to another
function clickStart(hide, show) {
  document.getElementById(hide).style.display = "none";
  document.getElementById(show).style.display = "block";
  window.scrollTo(0, 0);
}

//changes inner HTML of div with ID=x to y
function change(x, y) {
  document.getElementById(x).innerHTML = y;
}

//adds y to inner HTML of div with ID=x
function addToDiv(x, y) {
  document.getElementById(x).innerHTML += y;
}

//Function to randomly shuffle an array:
function shuffle(o) { //v1.0
  for (var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
  return o;
};

//Randomly sample n values from an array
function getRandomSubarray(arr, size) {
  var shuffled = arr.slice(0),
    i = arr.length,
    temp, index;
  while (i--) {
    index = Math.floor((i + 1) * Math.random());
    temp = shuffled[index];
    shuffled[index] = shuffled[i];
    shuffled[i] = temp;
  }
  return shuffled.slice(0, size);
}

//Needs to be rewritten, so the participants don't download it, but we save it.
function saveText(text, filename){
  var a = document.createElement('a');
  a.setAttribute('href', 'data:text/plain;charset=utf-u,'+encodeURIComponent(text));
  a.setAttribute('download', filename);
  a.click()
}


//load JSON file
function loadJSON(file, callback) {
  var rawFile = new XMLHttpRequest();
  rawFile.overrideMimeType("application/json");
  rawFile.open("GET", file, true);
  rawFile.onreadystatechange = function () {
    if (rawFile.readyState === 4 && rawFile.status == "200") {
      callback(rawFile.responseText);
    }
  }
  rawFile.send(null);
}

//Create normal noise distribution
function myNorm() {
  var x1, x2, rad, c;
  do {
    x1 = 2 * Math.random() - 1;
    x2 = 2 * Math.random() - 1;
    rad = x1 * x1 + x2 * x2;
  } while (rad >= 1 || rad == 0);
  c = Math.sqrt(-2 * Math.log(rad) / rad);
  return (x1 * c);
};

//average the values in an array
function average(inputArray) {
  var total = 0
  for (var i = 0; i < inputArray.length; i++) {
    total += inputArray[i];
  }
  var avg = total / inputArray.length;
  return avg;
};

//Convert cumulative score to reward value
function rewardCum(scoreTotal) {
  var r = 0,
    r_i;
  for (var i = 0; i < scoreTotal.length; i++) {
    r_i = scoreTotal[i] / (scale[i] + 5) / 300 * 1.5;
    r = r + r_i
  }
  if (r > 1.5) {
    r = 1.5; //limit to max reward, in case of any funny business
  }
  return toFixed(r, 2);
}


//single trial reward
function performanceScore(points, scale) {
  var r = 0;
  //cumulative regret (as a percentage)
  r = points / ((scale + 5) * horizon);
  return toFixed(r * 100);
}

function finalPerformance(scoreArray) {
  var finalScore = 0;
  for (i = 0; i < scoreArray.length; i++) { //loop through score array
    finalScore += parseInt(performanceScore(parseInt(scoreArray[i]), parseInt(scale[i])));
  }
  return toFixed(finalScore / scoreArray.length)
}



//random number generator
function randomNum(min, max) {
  return Math.floor(Math.random() * (max - min + 1) + min)
}

//Display a float to a fixed percision
function toFixed(value, precision) {
  var precision = precision || 0,
    power = Math.pow(10, precision),
    absValue = Math.abs(Math.round(value * power)),
    result = (value < 0 ? '-' : '') + String(Math.floor(absValue / power));

  if (precision > 0) {
    var fraction = String(absValue % power),
      padding = new Array(Math.max(precision - fraction.length, 0) + 1).join('0');
    result += '.' + padding + fraction;
  }
  //console.log(result);
  return result;
}

var fullurl = window.location.href;

// extract URL parameters
function turkGetParam(name) {
  var regexS = "[\?&]" + name + "=([^&#]*)";
  var regex = new RegExp(regexS);
  var tmpURL = fullurl;
  var results = regex.exec(tmpURL);
  if (results == null) {
    return "";

  } else {
    return results[1];
  }
}

//var workerID=turkGetParam('workerId');


function getAge(birthDate) {

  var dob = new Date(birthDate);
  var today = new Date();
  var age2 = today.getFullYear() - dob.getFullYear();
  var m = today.getMonth() - dob.getMonth();
  if (m < 0 || (m === 0 && today.getDate() < dob.getDate())) age2--;
  return age2;
}

var timeEnded = false;

function startTimer(duration, display) {
    var timer = duration, minutes, seconds;
    var MyTimer = setInterval(function () {
        minutes = parseInt(timer / 60, 10) +1;
        seconds = parseInt(timer % 60, 10);

        seconds = seconds < 10 ? "0" + seconds : seconds;

        if (--timer <= 0) {
          clearInterval(MyTimer);
          timeEnded = true;

        }
    }, 1000);



}


//END
