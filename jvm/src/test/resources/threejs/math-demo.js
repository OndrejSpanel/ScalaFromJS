function demo() {
    var vx = new Vector3(1, 0, 0);
    var vy = new Vector3(0, 1, 0);
    vx.dot(vy);

    var q0 = new Quaternion(0, 0, 0, 1);
    q0.dot(q0);

    vy.applyAxisAngle(vx, 0)
}
