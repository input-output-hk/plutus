import "./index.css";

function initializeFaqComponent() {
  const faqElements = document.getElementsByClassName("faq");
  for (const faqComponent of faqElements) {
    faqComponent.addEventListener("click", () => faqComponent.classList.toggle("expanded"));
  }
}

function initializeBackToTopComponent (){
  myID = document.getElementById("myID");

      var myScrollFunc = function () {
        var y = window.scrollY;
        if (y >= 400) {
          myID.className = "back-to-top show"
        } else {
          myID.className = "back-to-top hide"
        }
      };

      window.addEventListener("scroll", myScrollFunc);
}

window.onload = function () {
  initializeFaqComponent();
  initializeBackToTopComponent ();
};

