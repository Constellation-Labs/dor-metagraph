import express, { Express, Request, Response } from "express";
import bodyParser from "body-parser";
import dotenv from "dotenv";

import devicesRoutes from './src/routes/devices.route'

dotenv.config();

const app: Express = express();
const port = 3333;

app.use(bodyParser.json());
app.use(
  bodyParser.urlencoded({
    extended: true,
  })
);

app.get("/", (req: Request, res: Response) => {
  return res.json({ message: "ok" });
});

app.use('/metagraph/devices', devicesRoutes);

app.listen(port, () => {
  console.log(`⚡️[server]: Server is running at http://localhost:${port}`);
});
