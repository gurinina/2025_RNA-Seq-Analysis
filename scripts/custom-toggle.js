document.addEventListener("DOMContentLoaded", function() {
  document.querySelectorAll("pre.fold-hide").forEach(function(pre) {
    // Create a toggle button
    const button = document.createElement("button");
    button.className = "fold-hide-toggle";
    button.innerText = "Show Code";

    // Insert the button before the hidden code block
    pre.before(button);

    // Toggle visibility of the code block
    button.addEventListener("click", function() {
      if (pre.style.display === "none") {
        pre.style.display = "block";
        button.innerText = "Hide Code";
      } else {
        pre.style.display = "none";
        button.innerText = "Show Code";
      }
    });
  });
});
