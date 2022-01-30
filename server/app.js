const express = require('express')
const app = express()
const path = require('path');


let publicDirPath = path.join(__dirname + "/../", 'public');
app.use(express.static(publicDirPath));
app.get('/', (req, res) => {
    res.sendFile(join(publicDirPath, "index.html"))
}).get('/role/*', (req, res) => {
    res.sendFile(join(publicDirPath, "index.html"))
})

const port = process.env.PORT || 3000
app.listen(port, () => {
    console.log(`Live at http://0.0.0.0:${port}`)
});
