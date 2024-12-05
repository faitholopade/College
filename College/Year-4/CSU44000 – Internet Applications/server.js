const express = require("express");
const axios = require("axios");
const app = express();
const PORT = 3000;
const API_KEY = "9d921cb9dfb387f4f3dc01736f63d7c4";

// Enable CORS to allow cross-origin requests
app.use((req, res, next) => {
  res.header("Access-Control-Allow-Origin", "*");
  res.header(
    "Access-Control-Allow-Headers",
    "Origin, X-Requested-With, Content-Type, Accept"
  );
  next();
});

// Fetch weather by city name
app.get("/weather/:city", async (req, res) => {
  const city = req.params.city;
  try {
    const response = await axios.get(
      `https://api.openweathermap.org/data/2.5/forecast?q=${city}&units=metric&appid=${API_KEY}`
    );
    res.json(response.data);
  } catch (error) {
    console.error(error.message);
    res.status(500).json({ error: "Error fetching weather data" });
  }
});

// Fetch weather by location (latitude & longitude)
app.get("/weather/location", async (req, res) => {
  const { lat, lon } = req.query;
  if (!lat || !lon) {
    return res
      .status(400)
      .json({ error: "Latitude and longitude are required" });
  }
  try {
    const response = await axios.get(
      `https://api.openweathermap.org/data/2.5/onecall`,
      {
        params: {
          lat,
          lon,
          exclude: "minutely,hourly,alerts",
          units: "metric",
          appid: API_KEY,
        },
      }
    );
    res.json(response.data);
  } catch (error) {
    console.error("Error fetching weather data:", error.message);
    res.status(500).json({ error: "Error fetching weather data" });
  }
});

// Fetch air pollution data
app.get("/air-pollution", async (req, res) => {
  const { lat, lon } = req.query;
  try {
    const response = await axios.get(
      `http://api.openweathermap.org/data/2.5/air_pollution`,
      {
        params: { lat, lon, appid: API_KEY },
      }
    );
    res.json(response.data);
  } catch (error) {
    console.error("Error fetching air pollution data:", error.message);
    res.status(500).json({ error: "Error fetching air pollution data" });
  }
});

// Fetch city suggestions based on user input
app.get("/city-suggestions", async (req, res) => {
  const query = req.query.query;
  try {
    const response = await axios.get(
      `http://api.openweathermap.org/geo/1.0/direct?q=${query}&limit=5&appid=${API_KEY}`
    );
    res.json(response.data);
  } catch (error) {
    console.error("Error fetching city suggestions:", error.message);
    res.status(500).json({ error: "Error fetching city suggestions" });
  }
});

// Start the server
app.listen(PORT, () => {
  console.log(`Server is running on http://localhost:${PORT}`);
});
