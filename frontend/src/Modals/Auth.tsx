import React, { useState, type FormEvent, type MouseEvent } from "react";
import { useAuth } from "../Context/AuthContext"; 
import { useNavigate } from "react-router-dom";

export type AuthMode = "login" | "signup";

interface AuthModalProps {
  mode: AuthMode;                     // Whether the modal is in login or signup mode
  onClose: () => void;                 // Close the modal
  onModeChange: (mode: AuthMode) => void; // Switch between login/signup
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

  // Handle login/signup submission
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
        // Attempt registration
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
        // Auto-login after successful signup
        await login(email, password);
      } else {
        // Login
        await login(email, password);
      }

      // Success — close modal and go to dashboard
      onClose(); 
      navigate("/dashboard");

    } catch (err) {
      // Show error message in UI
      setError((err as Error).message);
    } finally {
      setLoading(false);
    }
  };

  // Prevent closing modal when clicking inside it
  const stopPropagation = (e: MouseEvent<HTMLDivElement>) => e.stopPropagation();

  return (
    <div
      className="fixed inset-0 bg-black/50 flex justify-center items-center z-50"
      onClick={onClose}
    >
      <div
        className="bg-white rounded-lg p-8 w-96 relative shadow-xl"
        onClick={stopPropagation}
      >
        {/* Title */}
        <h2 className="text-2xl mb-4 text-blue-500 font-medium">
          {isSignup ? "Sign Up" : "Log In"}
        </h2>

        {/* Form */}
        <form onSubmit={handleSubmit} className="flex flex-col space-y-4">
          {isSignup && (
            <input
              type="text"
              placeholder="Name"
              required
              value={name}
              onChange={(e) => setName(e.target.value)}
              className="p-2 rounded text-gray-700 bg-gray-100 border border-gray-300 focus:outline-none focus:ring-2 focus:ring-blue-400"
            />
          )}
          <input
            type="email"
            placeholder="Email"
            required
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            className="p-2 rounded text-gray-700 bg-gray-100 border border-gray-300 focus:outline-none focus:ring-2 focus:ring-blue-400"
          />
          <input
            type="password"
            placeholder="Password"
            required
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            className="p-2 rounded text-gray-700 bg-gray-100 border border-gray-300 focus:outline-none focus:ring-2 focus:ring-blue-400"
          />
          {isSignup && (
            <input
              type="password"
              placeholder="Confirm Password"
              required
              value={confirmPassword}
              onChange={(e) => setConfirmPassword(e.target.value)}
              className="p-2 rounded text-gray-700 bg-gray-100 border border-gray-300 focus:outline-none focus:ring-2 focus:ring-blue-400"
            />
          )}

          {/* Error message */}
          {error && <p className="text-red-500 text-sm text-center">{error}</p>}

          {/* Switch between login and signup */}
          <button
            className="text-blue-500 text-sm"
            type="button"
            onClick={() => onModeChange(isSignup ? "login" : "signup")}
          >
            {isSignup
              ? "Already have an account? Log in instead."
              : "Don't have an account? Sign up now!"}
          </button>

          {/* Submit */}
          <button
            type="submit"
            disabled={loading}
            className="bg-blue-500 hover:bg-blue-600 p-2 rounded text-white disabled:bg-gray-400 transition-colors"
          >
            {loading ? "Please wait..." : isSignup ? "Sign Up" : "Log In"}
          </button>
        </form>

        {/* Close button */}
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
