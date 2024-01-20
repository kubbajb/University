function time() {
  const options = {
    hour: "numeric",
    minute: "numeric",
    second: "numeric",
    hour12: false
  };
  return new Date().toLocaleString("en-US", options);
}