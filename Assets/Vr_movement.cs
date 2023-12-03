using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Vr_movement : MonoBehaviour
{
    public Transform vrCamera;
    public float speed = 3.0f;
    public float maxVerticalAngle = 25.0f; 
    public float minVerticalAngle = -25.0f; 

    private CharacterController cc;

    void Start()
    {
        cc = GetComponent<CharacterController>();
    }

    void Update()
    {
        if (vrCamera.eulerAngles.x < minVerticalAngle || vrCamera.eulerAngles.x > maxVerticalAngle)
        {
            Vector3 forward = vrCamera.TransformDirection(Vector3.forward);
            forward.y = 0; 

            Vector3 moveDirection = forward * speed;
            forward.Normalize();

            cc.SimpleMove(moveDirection);
        }
        else
        {
            // Stop the camera movement
            Vector3 moveDirection = Vector3.zero;
            cc.SimpleMove(moveDirection);
        }
    }
}
