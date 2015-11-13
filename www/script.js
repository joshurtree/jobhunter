
function saveJob(element, url) { 
  Shiny.onInputChange('savejob', url);  
  linkVisibility(element, 0, 1);
}

function deleteJob(element, url) { 
  Shiny.onInputChange('deletejob', url); 
  linkVisibility(element, 1, 0);
}

// if the url given is in the list of jobs saved the change visibility of links
function setupLinks(row, data) {
  if (savedUrls.indexOf(data[0]) != -1) {
    linkVisibility(row.getElementsByTagName('td')[3], 0, 1);
  } else {
    linkVisibility(row.getElementsByTagName('td')[3], 1, 0);
  }
}

//Change the visibility of the save/delete job links
function linkVisibility(element, visibleElement, hiddenElement) {
    var links = element.getElementsByTagName('a'); 
    links[visibleElement].style.display = 'none'; 
    links[hiddenElement].style.display = 'initial';
}
