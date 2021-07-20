import "./index.css";
import $ from "jquery";

function initializeFaqComponent() {
  const faqElements = document.getElementsByClassName("faq");
  for (const faqComponent of faqElements) {
    faqComponent.addEventListener("click", () => faqComponent.classList.toggle("expanded"));
  }
}

function initializeBackToTopComponent() {
  const backToTopComponent = document.getElementById("back-to-top");

  var myScrollFunc = function () {
    var y = window.scrollY;
    if (y >= 400) {
      backToTopComponent.className = "show select-none";
    } else {
      backToTopComponent.className = "hide";
    }
  };

  window.addEventListener("scroll", myScrollFunc);
}

// This function adds smooth scrolling for the internal links.
// It is using some deprecated features from jQuery which adds 80kb to the build.
// TODO: maybe refactor to use native JS.
function initializeSmoothScrolling() {
  // Select all links with hashes
  $('a[href*="#"]')
    // Remove links that don't actually link to anything
    .not('[href="#"]')
    .not('[href="#0"]')
    .click(function (event) {
      // On-page links
      if (
        location.pathname.replace(/^\//, "") == this.pathname.replace(/^\//, "") &&
        location.hostname == this.hostname
      ) {
        // Figure out element to scroll to
        var target = $(this.hash);
        target = target.length ? target : $("[name=" + this.hash.slice(1) + "]");
        // Does a scroll target exist?
        if (target.length) {
          // Only prevent default if animation is actually gonna happen
          event.preventDefault();
          $("html, body").animate(
            {
              scrollTop: target.offset().top,
            },
            500,
            function () {
              // Callback after animation
              // Must change focus!
              var $target = $(target);
              $target.focus();
              if ($target.is(":focus")) {
                // Checking if the target was focused
                return false;
              } else {
                $target.attr("tabindex", "-1"); // Adding tabindex for elements not focusable
                $target.focus(); // Set focus again
              }
            }
          );
        }
      }
    });
}

window.onload = function () {
  initializeFaqComponent();
  initializeBackToTopComponent();
  initializeSmoothScrolling();
};
