import "./index.css";

function initializeFaqComponent() {
  const faqElements = document.getElementsByClassName("faq");
  for (const faqComponent of faqElements) {
    faqComponent.addEventListener("click", () => faqComponent.classList.toggle("expanded"));
  }
}

// function initializeHeaderComponent() {
//   const header = document.getElementsByTagName("header")[0];
//   const headerHeight = header.getBoundingClientRect().height;
//   const mainTryButtons = document.getElementById("main-try-buttons");

//   window.onscroll = function () {
//     // The header always have fixed position and as soon as the scroll moves we add
//     // a background to it
//     if (window.scrollY > 0) {
//       header.classList.add("bg-blured");
//     } else {
//       header.classList.remove("bg-blured");
//     }

//     // The header "try Run/Build" buttons appear when the same buttons in the main section
//     // are no longer visible
//     const mainButtonBoundingBox = mainTryButtons.getBoundingClientRect();
//     if (headerHeight > mainButtonBoundingBox.y + mainButtonBoundingBox.height) {
//       header.classList.add("with-buttons");
//     }
//     // and dissapear once the main buttons are fully visible again
//     if (headerHeight <= mainButtonBoundingBox.y) {
//       header.classList.remove("with-buttons");
//     }
//   };
// }
// window.onload = function () {
//   initializeFaqComponent();
//   initializeHeaderComponent();
// };
