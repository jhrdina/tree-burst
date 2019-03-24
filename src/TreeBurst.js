// This file is written directly in JS because we need exact control over imports order to ensure that the following module gets imported before everything else:
import "./BootstrapNewMUIStyles";
import "./TreeBurstApp.bs";

if ("serviceWorker" in navigator) {
  window.addEventListener("load", () => {
    navigator.serviceWorker
      .register("/service-worker.js")
      .then(registration => {
        console.log("SW registered.");
      })
      .catch(registrationError => {
        console.log("SW registration failed: ", registrationError);
      });
  });
}
