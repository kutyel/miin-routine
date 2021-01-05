import "./main.css";
import "firebase/auth";
import "regenerator-runtime/runtime";
import "@webcomponents/webcomponentsjs/custom-elements-es5-adapter";
import { Elm } from "./Main.elm";
import { auth, initializeApp } from "firebase/app";
import { GoogleChart } from "@google-web-components/google-chart";

class Calendar extends GoogleChart {
  static get properties() {
    return { dateRows: { type: Array, observer: "_dateRowsChanged" } };
  }
  _dateRowsChanged(data) {
    super.rows = data.map(([date, value, tooltip]) => [
      new Date(date),
      value,
      tooltip,
    ]);
  }
}

customElements.define("google-calendar", Calendar);

initializeApp({
  apiKey: process.env.ELM_APP_API_KEY,
  authDomain: process.env.ELM_APP_AUTH_DOMAIN,
  projectId: process.env.ELM_APP_PROJECT_ID,
});

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: [
    process.env.ELM_APP_API_KEY,
    process.env.ELM_APP_PROJECT_ID,
    [window.innerHeight, window.innerWidth],
  ],
});

app.ports.signIn.subscribe(() =>
  auth()
    .signInWithPopup(new auth.GoogleAuthProvider())
    .then(({ user }) =>
      user
        .getIdToken()
        .then((token) =>
          app.ports.signInInfo.send({ token, email: user.email, uid: user.uid })
        )
    )
    .catch((error) => app.ports.signInError.send(error))
);

app.ports.signOut.subscribe(() => auth().signOut());

auth().onAuthStateChanged(
  (user) =>
    !!user &&
    user
      .getIdToken()
      .then((token) =>
        app.ports.signInInfo.send({ token, email: user.email, uid: user.uid })
      )
      .catch((error) => app.ports.signInError.send(error))
);
