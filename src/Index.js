// This file is written directly in JS because we need exact control over imports order to ensure that the following module gets imported before everything else:
import "./BootstrapNewMUIStyles";
import "./App.bs";

if (process.env.NODE_ENV === "production" && "serviceWorker" in navigator) {
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
