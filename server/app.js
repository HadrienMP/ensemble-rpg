const express = require('express')
const app = express()
const path = require('path');


let publicDirPath = path.join(__dirname + "/../", 'public');
app.use(express.static(publicDirPath));
app.get('*', (req, res) => {
    res.sendFile(path.join(publicDirPath, "index.html"))
}).get(/^(^api).*/, (req, res) => {
    res.sendFile(path.join(publicDirPath, "index.html"))
})

const port = process.env.PORT || 1234
app.listen(port, () => {
    console.log(`Live at http://0.0.0.0:${port}`)
});
