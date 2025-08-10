import React, { useState, type FormEvent, type MouseEvent } from "react";
import { useAuth } from "../Context/AuthContext"; // Import the hook

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

  // Get the login function from our central context
  const { login } = useAuth();

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
        // NOTE: The actual registration fetch would still happen here.
        // After a successful registration, you then log the user in.
        const registerRes = await fetch("http://localhost:8080/register", {
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
        
        // After successful registration, log the user in using the context function
        await login(email, password);

      } else {
        // For login, just call the function from the context
        await login(email, password);
      }

      setLoading(false);
      onClose(); // Close the modal on success
    } catch (err) {
      setError((err as Error).message);
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
          {error && <p className="text-red-500">{error}</p>}
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
            className="bg-blue-500 hover:bg-blue-600 p-2 rounded text-white"
          >
            {loading ? "Please wait..." : isSignup ? "Sign Up" : "Log In"}
          </button>
        </form>
        <button
          className="absolute top-2 right-4 text-gray-400 hover:text-white"
          onClick={onClose}
          aria-label="Close modal"
        >
          &#x2715;
        </button>
      </div>
    </div>
  );
};