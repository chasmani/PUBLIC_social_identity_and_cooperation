console.log("In custom Script");

var bigGroupDragged = false;
var myGroupDragged = false;
var otherGroupDragged = false;



function trackDragged(groupLabel) {
	console.log("In custom script");
	console.log("It's Dragged");
	console.log(groupLabel);

	if (groupLabel == "Allshire") {
		bigGroupDragged = true;
	}

	if (groupLabel == "Westville") {
		myGroupDragged = true;
	}

	if (groupLabel == "Eastburgh") {
		otherGroupDragged = true;
	}

	if (bigGroupDragged && myGroupDragged && otherGroupDragged) {
		document.getElementsByClassName("otree-btn-next")[0].style.display = "block";

	}



}