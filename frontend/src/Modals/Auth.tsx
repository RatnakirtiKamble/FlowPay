import React, { useState, type FormEvent, type MouseEvent } from "react";
import { useAuth } from "../Context/AuthContext"; 
import { useNavigate } from "react-router-dom";

export type AuthMode = "login" | "signup";

interface AuthModalProps {
  mode: AuthMode;
  onClose: () => void;
  onModeChange: (mode: AuthMode) => void;
}

export const AuthModal: React.FC<AuthModalProps> = ({
  mode,
  onClose,
  onModeChange,
}) => {
  const [name, setName] = useState("");
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [confirmPassword, setConfirmPassword] = useState("");
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  const { login } = useAuth();
  const navigate = useNavigate();

  const isSignup = mode === "signup";

  const handleSubmit = async (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    setError(null);

    if (isSignup && password !== confirmPassword) {
      setError("Passwords do not match");
      return;
    }

    setLoading(true);

    try {
      if (isSignup) {
        // Corrected registration fetch call
        const registerRes = await fetch(`${import.meta.env.VITE_BACKEND_URL}/register`, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            reqName: name,
            reqEmail: email,
            reqPassword: password,
          }),
        });

        if (!registerRes.ok) {
            const errorData = await registerRes.json().catch(() => ({}));
            throw new Error(errorData.message || "Registration failed.");
        }
        // After successful registration, log the user in
        await login(email, password);

      } else {
        // Attempt to log in
        await login(email, password);
      }

      // ---- SUCCESS CASE ----
      // This code only runs if login() was successful (did not throw an error).
      // Now we can safely close the modal and navigate.
      onClose(); 
      navigate("/dashboard");

    } catch (err) {
      // ---- FAILURE CASE ----
      // If login() threw an error, it gets caught here.
      // We set the local error state to display the message in the modal's UI.
      setError((err as Error).message);
    } finally {
      setLoading(false);
    }
  };

  const stopPropagation = (e: MouseEvent<HTMLDivElement>) => e.stopPropagation();

  return (
    <div
      className="fixed inset-0 bg-black/50 flex justify-center items-center z-50"
      onClick={onClose}
    >
      <div
        className="bg-white rounded-lg p-8 w-96 relative"
        onClick={stopPropagation}
      >
        <h2 className="text-2xl mb-4 text-blue-500 font-medium">
          {isSignup ? "Sign Up" : "Log In"}
        </h2>
        <form onSubmit={handleSubmit} className="flex flex-col space-y-4">
          {isSignup && (
            <input
              type="text"
              placeholder="Name"
              required
              value={name}
              onChange={(e) => setName(e.target.value)}
              className="p-2 rounded text-gray-500 bg-gray-200 border-gray-100"
            />
          )}
          <input
            type="email"
            placeholder="Email"
            required
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            className="p-2 rounded text-gray-500 bg-gray-200 border-gray-100"
          />
          <input
            type="password"
            placeholder="Password"
            required
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            className="p-2 rounded text-gray-500 bg-gray-200 border-gray-100"
          />
          {isSignup && (
            <input
              type="password"
              placeholder="Confirm Password"
              required
              value={confirmPassword}
              onChange={(e) => setConfirmPassword(e.target.value)}
              className="p-2 rounded text-gray-500 bg-gray-200 border-gray-100"
            />
          )}
          {error && <p className="text-red-500 text-sm text-center">{error}</p>}
          <button
            className="text-blue-500 text-sm"
            type="button"
            onClick={() => onModeChange(isSignup ? "login" : "signup")}
          >
            {isSignup
              ? "Already have an account? Log in instead."
              : "Don't have an account? Sign up now!"}
          </button>
          <button
            type="submit"
            disabled={loading}
            className="bg-blue-500 hover:bg-blue-600 p-2 rounded text-white disabled:bg-gray-400"
          >
            {loading ? "Please wait..." : isSignup ? "Sign Up" : "Log In"}
          </button>
        </form>
        <button
          className="absolute top-2 right-4 text-gray-400 hover:text-gray-600"
          onClick={onClose}
          aria-label="Close modal"
        >
          &#x2715;
        </button>
      </div>
    </div>
  );
};
