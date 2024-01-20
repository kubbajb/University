
var intervalId = window.setInterval(function(){
  time()
}, 2000);

function time() {
  const options = {
    year: "numeric",
    month: "numeric",
    day: "numeric",
    hour: "numeric",
    minute: "numeric",
    second: "numeric",
    hour12: false
  };
  return new Date().toLocaleString("pl-PL", options);
}
