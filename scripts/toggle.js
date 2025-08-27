document.addEventListener("DOMContentLoaded", function() {
  document.querySelectorAll("details").forEach(function(details) {
    // Listen for toggle events
    details.addEventListener("toggle", function() {
      const visContainer = this.querySelector(".vis-container");

      if (visContainer) {
        if (this.open) {
          visContainer.style.display = "block";
        } else {
          visContainer.style.display = "none";
        }
      }
    });
  });
});
