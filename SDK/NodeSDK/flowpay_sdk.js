const axios = require('axios');

class FlowPayClient {
    /**
     * A simple Node.js SDK client for interacting with the FlowPay API.
     *
     * @example
     * const main = async () => {
     * try {
     * const client = new FlowPayClient({ apiKey: "sk_your_api_key_here" });
     * const response = await client.makePayment(100.50);
     * console.log("Payment successful:", response);
     * } catch (error) {
     * console.error(`Payment failed: ${error.message}`);
     * }
     * };
     * main();
     */
    constructor({ apiKey, baseUrl = "http://localhost:8080" }) {
        /**
         * Initializes the FlowPay client.
         *
         * @param {string} apiKey - The merchant's secret API key (prefixed with 'sk_').
         * @param {string} [baseUrl="http://localhost:8080"] - The base URL of the FlowPay API.
         */
        if (!apiKey) {
            throw new Error("API key is required.");
        }

        // Create an axios instance with pre-configured settings
        this.api = axios.create({
            baseURL: baseUrl,
            headers: {
                "Content-Type": "application/json",
                "X-API-Key": apiKey,
            },
        });
    }

    /**
     * Creates a new payment.
     *
     * @param {number} amount - The amount for the payment. Must be positive.
     * @returns {Promise<object>} A promise that resolves to an object containing the
     * payment response from the API.
     * @throws {Error} If the payment amount is not positive, or if an API/network error occurs.
     */
    async makePayment(amount) {
        if (amount <= 0) {
            throw new Error("Payment amount must be a positive number.");
        }

        const payload = { paymentAmount: amount };

        try {
            const response = await this.api.post('/payments', payload);
            return response.data;
        } catch (error) {
            // Handle axios-specific errors to provide better feedback
            if (error.response) {
                // The request was made and the server responded with a status code
                // that falls out of the range of 2xx
                const errorDetails = error.response.data || error.response.statusText;
                throw new Error(`API Error (Status ${error.response.status}): ${errorDetails}`);
            } else if (error.request) {
                // The request was made but no response was received
                throw new Error("A network error occurred: The server did not respond.");
            } else {
                // Something happened in setting up the request that triggered an Error
                throw new Error(`An unexpected error occurred: ${error.message}`);
            }
        }
    }
}

module.exports = { FlowPayClient };
