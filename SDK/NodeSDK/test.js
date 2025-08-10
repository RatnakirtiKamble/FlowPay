const {FlowPayClient} = require('./flowpay_sdk');

const main = async () => {
    try {
        const client = new FlowPayClient({ apiKey: "sk_your_api_key_here" });
        const response = await client.makePayment(100.50);
        console.log("Payment successful:", response);
    } 
    catch (error) {
        console.error(`Payment failed: ${error.message}`);
    }
};

main();