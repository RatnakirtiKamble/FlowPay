import requests
from typing import Dict, Any

class FlowPayClient:
    """
    A simple Python SDK client for interacting with the FlowPay API.

    Usage:
        client = FlowPayClient(api_key="sk_your_api_key_here")
        try:
            response = client.make_payment(amount=100.50)
            print("Payment successful:", response)
        except Exception as e:
            print(f"Payment failed: {e}")
    """

    def __init__(self, api_key: str, base_url: str = "https://flowpay-production.up.railway.app"):
        """
        Initializes the FlowPay client.

        Args:
            api_key (str): The merchant's secret API key (prefixed with 'sk_').
            base_url (str, optional): The base URL of the FlowPay API.
                                      Defaults to "http://localhost:8080".
        
        Raises:
            ValueError: If the API key is not provided.
        """
        if not api_key:
            raise ValueError("API key is required.")

        self.base_url = base_url
        self._session = requests.Session()
        self._session.headers.update({
            "Content-Type": "application/json",
            "X-API-Key": api_key
        })

    def make_payment(self, amount: float) -> Dict[str, Any]:
        """
        Creates a new payment.

        Args:
            amount (float): The amount for the payment. Must be positive.

        Returns:
            Dict[str, Any]: A dictionary containing the payment response from the API,
                            including transaction_id, status, and new_balance.

        Raises:
            ValueError: If the payment amount is not positive.
            requests.exceptions.RequestException: For network-related errors.
            Exception: For API-level errors (e.g., invalid key, insufficient funds).
        """
        if amount <= 0:
            raise ValueError("Payment amount must be a positive number.")

        payment_url = f"{self.base_url}/payments"
        payload = {"paymentAmount": amount}

        try:
            response = self._session.post(payment_url, json=payload)
            
            response.raise_for_status()

            return response.json()

        except requests.exceptions.HTTPError as http_err:
            error_message = "An unknown error occurred."
            try:
                error_details = response.text
                error_message = f"API Error (Status {response.status_code}): {error_details}"
            except Exception:
                pass
            raise Exception(error_message) from http_err
        
        except requests.exceptions.RequestException as req_err:
            raise Exception(f"A network error occurred: {req_err}") from req_err