export var PathState;
(function (PathState) {
    PathState[PathState["Taken"] = 0] = "Taken";
    PathState[PathState["Future"] = 1] = "Future";
    PathState[PathState["Pruned"] = 2] = "Pruned";
})(PathState || (PathState = {}));
